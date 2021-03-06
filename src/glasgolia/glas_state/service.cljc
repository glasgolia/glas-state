(ns glasgolia.glas-state.service
  (:require [glasgolia.glas-state.stateless :as sl]
            [clojure.core.async :as as]))


(def service-logger
  "Simple state logger that you can add as a :change-listener
   in the config when creating a new service.
   This just logs state changes using println"
  (fn [{:keys [prev new]}]
    (let [prev-value (:value prev)
          new-value (:value new)
          prev-context (:context prev)
          new-context (:context new)]
      (when (not= prev-value new-value)
        (println "VALUE: " new-value " <-- " prev-value))
      (when (not= prev-context new-context)
        (println "CONTEXT: " new-context " <-- " prev-context)))))

(defn create-service-logger [service-name]
  (fn [{:keys [prev new]}]
    (let [prev-value (:value prev)
          new-value (:value new)
          prev-context (:context prev)
          new-context (:context new)]
      (when (not= prev-value new-value)
        (println (str "[" service-name "]") "VALUE: " new-value " <-- " prev-value))
      (when (not= prev-context new-context)
        (println (str "[" service-name "]") "CONTEXT: " new-context " <-- " prev-context)))))

(declare dispatch)

(defn- start-activities [{:keys [storage machine] :as service} context action]
  (let [activities (:ids action)
        running (:activities @storage)]
    (doseq [activity-event activities]
      (let [id (sl/event-type activity-event)
            started? (contains? running id)]
        (when (not started?)
          (let [fun (:activity-functions machine)
                stop-fun (when fun (fun context activity-event))]
            (if stop-fun
              (swap! (:activities service) assoc id stop-fun))
            (swap! storage (fn [state]
                             (assoc state :activities (conj (:activities state) id))))
            ))))))

(defn stop-activities [{:keys [storage] :as service} context action]
  (let [activities (:ids action)
        running (:activities @storage)]
    (doseq [activity-event activities]
      (let [id (sl/event-type activity-event)
            started? (contains? running id)]
        (when started?
          (let [stop-fun @(:activities service)]
            (when stop-fun
              (swap! (:activities service) dissoc id)
              (stop-fun))
            (swap! storage (fn [state]
                             (assoc state :activities (dissoc (:activities state) id))))))))
    ))
(declare stop)
(defn- create-invoke-child-fn [service id src data invoke-event]
  (cond
    (map? src)
    (fn [callback on-event]
      (let [service-creator (:child-service-creator service)
            child-machine (sl/machine-options src {:context data})
            child-callback (fn [event]
                             (if (= (sl/event-type event) :done/.)
                               (callback {:type (keyword "done.invoke" (str "child." (name id)))
                                          :data (:data event)})
                               (callback event)))
            child-service (service-creator child-machine {:parent-callback child-callback})]
        (on-event (fn [event]
                    (dispatch child-service event)))
        (fn []
          (stop child-service)))
      )

    (fn? src)
    (src data invoke-event)))

(defn- create-invoke-child-id [src]
  (if (map? src)
    (:id src)                                               ; The src is a map -> must be a machine
    nil))
(defn- invoke-child [service context action]
  (let [invoke (:config action)

        src (:src invoke)
        src (if (keyword? src)
              (get-in service [:machine :services src])
              src)
        id (or (:id invoke) (create-invoke-child-id src))
        ;_
        ;_ (assert (fn? src) "Invoke src must be a function")
        data (:data invoke)
        data (if (fn? data)
               (data context action)
               data)
        invoke-fn (create-invoke-child-fn service id src data action)
        callback (fn [event] (dispatch service event))
        listener-atom (atom (fn [e] nil))
        on-event (fn [listener] (reset! listener-atom listener))
        cleanup-fn (invoke-fn callback on-event)
        child-service-def {:id         id
                           :listener   listener-atom
                           :cleanup-fn cleanup-fn}
        ]
    (swap! (:child-services service) assoc id child-service-def)
    context
    ))
(defn- dispatch-to-child-service [service child-service-id event]
  (let [child-services @(:child-services service)
        child-service (get child-services child-service-id)
        _ (assert child-service (str "Child service not found: " child-service-id))
        child-listener @(:listener child-service)]
    (when child-listener
      (child-listener event))))

(defn- invoke-child-cleanup [service context event]
  (let [child-services @(:child-services service)
        child-service (get child-services (:id event))
        cleanup-fn (:cleanup-fn child-service)]
    (when cleanup-fn (cleanup-fn))
    (swap! (:child-services service) dissoc (:id event))))

(defn- sync-action-handler [{:keys [machine send-channel] :as service} context action event]
  (try
    (if (keyword? action)
      (sync-action-handler service context (get (:actions machine) action) event)
      (do
        (cond
          (fn? action) (do (action context event) context)
          (map? action) (let [type (:type action)
                              exec (:exec action)]
                          (if exec
                            ;execute the function defined in-line in the machine
                            (exec context event)
                            (if (and (or (string? type) (keyword? type)) (= (namespace type) "glas-state"))
                              ;We have an internal action
                              (case type
                                :glas-state/assign-context
                                (let [fun (:assigner action)
                                      fun (if (fn? fun)
                                            fun
                                            (get (:actions machine) fun))]
                                  (if (nil? fun)
                                    (ex-info "Unknown assigner in : " action)
                                    (fun context event)))
                                :glas-state/send (let [new-event (:event action)
                                                       new-event (if (fn? new-event) (new-event context event) new-event)
                                                       delay-context (:delay-context action)]
                                                   (if delay-context
                                                     (as/go (as/>! @send-channel {:event         new-event
                                                                                  :delay-context delay-context}))
                                                     (as/go (as/>! @send-channel {:event new-event})))
                                                   context)
                                :glas-state/send-parent (let [new-event (:event action)
                                                              new-event (if (fn? new-event) (new-event context event) new-event)
                                                              parent-callback (:parent-send service)]
                                                          (if parent-callback
                                                            (parent-callback new-event)
                                                            context))
                                :glas-state/invoke (do (invoke-child service context action)
                                                       context)
                                :glas-state/invoke-cleanup (do (invoke-child-cleanup service context action)
                                                               context)
                                :glas-state/start-activities (do (start-activities service context action)
                                                                 context)
                                :glas-state/stop-activities (do (stop-activities service context action)
                                                                context)
                                (ex-info "Unknown internal action" {:action action}))
                              context)))
          :else context)))
    (catch #?(:clj  Exception
              :cljs :default) e (ex-info "Exception while executing statechart action!" {:machine-id (:id machine)
                                                                                         :action     action
                                                                                         :event      event} e))))
(defn- notify-reactions [reactions old-value new-value]
  (doseq [[_key reaction] reactions]
    (try
      (reaction old-value new-value)
      (catch #?(:clj  Exception
                :cljs :default) e (ex-info "Exception while notifying reactors of stateschart service!"
                                           {:prev-state old-value :new-state new-value} e))))
  )
(defn notify-listeners [{:keys [change-listener value-reactions context-reactions]} prev-state new-state]
  (let [old-value (:value prev-state)
        new-value (:value new-state)]
    (when-not (= old-value new-value)
      (notify-reactions @value-reactions old-value new-value)))
  (let [old-value (:context prev-state)
        new-value (:context new-state)]
    (when-not (= old-value new-value)
      (notify-reactions @context-reactions old-value new-value)))
  (when change-listener
    (try
      (change-listener {:prev prev-state :new new-state})
      (catch #?(:clj  Exception
                :cljs :default) e (ex-info "Exception while notifying listeners of stateschart service!"
                                           {:prev-state prev-state :new-state new-state} e)))))

(defn- send-actions [inst context actions event new-state-value]
  (let [action-handler (:action-handler inst)]
    (loop [c context
           a actions]
      (let [next-action (first a)]
        (if next-action
          (let [new-context (action-handler inst c next-action event #_{:action next-action
                                                                        :state  new-state-value})]
            (recur new-context (rest a)))
          c))
      )
    ))

(defn- init-service [{:keys [storage machine] :as service}]
  (let [new-state (let [prev-state @storage
                        {:keys [actions context value] :as state} (sl/start-machine machine)
                        new-context (send-actions service context actions :glas-state/start-machine value)
                        new-state (-> state
                                      (dissoc :actions)
                                      (assoc :context new-context))]
                    (notify-listeners service prev-state new-state)
                    new-state)]
    (reset! storage new-state)))


(defn- handle-event [{:keys [storage machine send-channel] :as inst} {:keys [event waiting-channel delay-context]}]
  "Main Event handler"
  (if (:delay delay-context)
    ;Send delayed event
    (let [id (or (:id delay-context) event)
          delay (:delay delay-context)
          cancel-channel (as/chan 1)
          sc @send-channel]
      (swap! (:delayed-events inst) assoc id cancel-channel)
      (as/go
        (let [[v _] (as/alts! [(as/timeout delay) cancel-channel])]
          (when (and sc (not v))
            (as/>! sc {:event event}))
          (as/close! cancel-channel)
          (swap! (:delayed-events inst) dissoc id)))
      )
    ;Not a delayed event
    (if (:to delay-context)
      ;Send to Child service
      (dispatch-to-child-service inst (:to delay-context) event)
      ;Send to this service
      (if (= :done/. (sl/event-type event))
        (when-let [callback (:parent-send inst)]
          (callback event))
        (swap! storage (fn [prev-state]
                         (let [context (:context prev-state)
                               new-state (sl/transition-machine machine prev-state event)
                               new-context (send-actions inst context (:actions new-state) event (:value new-state))
                               new-state (-> new-state
                                             (dissoc :actions)
                                             (assoc :context new-context))]
                           (notify-listeners inst prev-state new-state)
                           new-state))))))
  (when waiting-channel
    (as/close! waiting-channel)))

(defn start [{:keys [send-channel service-state] :as service}]
  "Starts a given service.
  This creates the initial state and possible execute
  the entry actions for this initial state.
  If the service was already started, nothing will happen."
  (when (not= @service-state :started)
    (let [sc (as/chan 20)]
      (reset! send-channel sc)
      (as/go-loop [event (as/<! sc)]
        (if event
          (do (handle-event service event)
              (recur (as/<! sc)))
          (do (as/close! sc)
              (reset! send-channel nil))
          )
        )
      (init-service service)
      (reset! service-state :started)))
  service)
(defn reset
  "reset the state to the initial state of the machine.
  Warning, exit actions are not called, only entry actions
  of the initial state."
  [service] (init-service service))


(defn stop [{:keys [send-channel delayed-events]}]
  "stop a running service, needed to cleanup
  internal structures."
  (swap! delayed-events (fn [events]
                          (doseq [[_ chan] events]
                            (as/close! chan))
                          {}))
  (as/close! @send-channel)
  (reset! send-channel nil))

(defn create-service
  "Create a new Statechart service.
  Needs a machine definition and an optional config map.
  The config map can contain:
  - :state-atom  the atom that will be used to store the state
  - :change-listener  listener that will be called on transitions.
  This will be called with a map with {:new {<the new state>} :prev {<the previous state>}
  - :parent-callback  callback function that takes an event.
  This will be called by the service when we have a send-parent(...)
  - :service-id  id of this service, default is the machine-id
  - :auto-start automatically start the machine on the first event, default true
  - :service-data user-data that needs to be linked to this service
  - :child-service-creator function used to create child machine services
  The config map is also passed to the machine using the machine-options function"
  ([the-machine {:keys [state-atom
                        change-listener
                        parent-callback
                        service-id
                        auto-start service-data
                        child-service-creator] :as config}]
   (when (and state-atom (nil? @state-atom)) (reset! state-atom {}))
   (let [
         result {:storage               (or state-atom (atom {}))
                 :machine               (sl/machine-options the-machine config)
                 :service-id            (or service-id (:id the-machine))
                 :service-state         (atom :idle)
                 :send-channel          (atom nil)
                 :change-listener       change-listener
                 :parent-send           parent-callback
                 :delayed-events        (atom {})
                 :action-handler        sync-action-handler
                 :child-services        (atom {})
                 :activities            (atom {})
                 :value-reactions       (atom [])
                 :context-reactions     (atom [])
                 :service-data          service-data
                 :child-service-creator (or child-service-creator create-service)}]
     (if (or (nil? auto-start) auto-start)
       (start result)
       result)
     ))
  ([the-machine]
   (create-service the-machine {})))

(defn service-value [{:keys [storage] :as _service}]
  "Return the current value of this service"
  (:value @storage))
(defn service-context [{:keys [storage] :as _service}]
  "Return the current context of this service"
  (:context @storage))

(defn- add-reaction

  ([reaction-atom reaction]
   (let [reaction-key (gensym)]
     (swap! reaction-atom conj [reaction-key reaction])
     (fn []
       (swap! reaction-atom (fn [l] (into [] (filter (fn [p] (not= (first p) reaction-key)) l)))))))
  ([reaction reaction-atom transformer]
   (if (vector? transformer)
     (add-reaction reaction-atom reaction (fn [v] (get-in v transformer)))
     (add-reaction reaction-atom (fn [old new]
                                   (let [old (transformer old)
                                         new (transformer new)]
                                     (when (not= old new)
                                       (reaction old new))))))))
(defn add-value-reaction
  "Add a reaction when there is a value state change.
 The reaction must be a function that takes the old value and the new value.
 The optional transformer can be a function that takes a value and transforms it,
 or a vector that will be used as the get-in path for values.
 This functions returns a function that can be called  to remove the reaction from the service."
  ([service reaction]
   (let [value (service-value service)]
     (reaction value value))
   (add-reaction (:value-reactions service) reaction))
  ([service reaction transformer]
   (let [value (transformer (service-value service))]
     (reaction value value))
   (add-reaction (:value-reactions service) reaction transformer)))

(defn add-context-reaction
  "Add a reaction when there is a context state change.
 The reaction must be a function that takes the old value and the new value.
 The optional transformer can be a function that takes a value and transforms it,
 or a vector that will be used as the get-in path for values.
 This functions returns a function that can be called  to remove the reaction from the service."
  ([service reaction]
   (let [value (service-context service)]
     (reaction value value))
   (add-reaction (:context-reactions service) reaction))
  ([service reaction transformer]
   (let [value (transformer (service-context service))]
     (reaction value value))
   (add-reaction (:context-reactions service) reaction transformer)))

(defn dispatch
  "Dispatch an event to this service.
  If you want to delay the handling of the event an amount of time,
  you can add a delay context map containing {:delay <delay in ms> :id <delay-id, used to cancel if needed>}"
  ([{:keys [send-channel machine] :as _service} event]
   (if @send-channel
     (as/go (as/>! @send-channel {:event event}))
     (ex-info "Call start before dispatching events" {:machine-id (:id machine) :event event}))
   nil)
  ([{:keys [send-channel machine]} event delay-context]
   (when (not @send-channel) (ex-info "Call start before dispatching events" {:machine-id (:id machine) :event event :delay delay-context}))
   (as/go (as/>! @send-channel {:event event :delay-context delay-context}))
   nil)
  )


#?(:clj
         (defn dispatch-and-wait [{:keys [send-channel machine]} event]
           "Dispatch an event to this service, but block until the event is handled.
            This function is mainly for testing and should probably not be used in production"
           (if @send-channel
             (let [wait-channel (as/chan 1)]
               (as/>!! @send-channel {:event           event
                                      :waiting-channel wait-channel})
               (as/<!! wait-channel))
             (ex-info "Call start before dispatching events" {:machine-id (:id machine) :event event})))

   :cljs ())
(defn service-state [{:keys [storage] :as _service}]
  "Return the full current state of this service"
  (:context @storage))



(comment
  (def delay-test-machine {:initial :red
                           :context {:count 0}
                           :states  {:red    {:on    {:timer [{:target :green
                                                               :cond   (fn [c e] (< (:count c 0) 2))}
                                                              :done]}
                                              :entry [(sl/assign (fn [c e]
                                                                   (update c :count inc)))
                                                      (sl/send-event :timer {:delay 1000 :id :the-timer})]}
                                     :green  {:on    {:timer :orange}
                                              :entry (sl/send-event :timer {:delay 1000 :id :the-timer})}
                                     :orange {:on    {:timer :red}
                                              :entry (sl/send-event :timer {:delay 1000 :id :the-timer})}
                                     :done   {:type :final}}})

  (def inst (let [state (atom {})]
              (-> (create-service delay-test-machine {:change-listener service-logger})
                  (start))
              ))
  (dispatch inst :timer)
  (dispatch inst :timer {:delay 5000})
  (stop inst)
  (start inst)
  (prn @(:delayed-events inst))
  (service-value inst)
  (prn inst)
  (let [state (atom {})
        inst (-> (create-service delay-test-machine {:state-atom      state
                                                     :change-listener service-logger})
                 (start))]
    (println "started")
    (dispatch inst :timer)
    (Thread/sleep 2000)
    (println "done"))

  )
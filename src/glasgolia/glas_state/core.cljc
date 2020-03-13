(ns glasgolia.glas-state.core
  (:require [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.impl.event-queue :as queue]
            [clojure.core.async :as ca]
            [glasgolia.glas-state.impl.utils :as utils]))


(declare dispatch stop)


(defn machine-options
  "Create a new machine definition by merging the
  :guards, :actions, :activity-functions :context and :service values
  in the options map"
  [machine options]
  (merge-with into machine {:guards             (:guards options)
                            :actions            (:actions options)
                            :activity-functions (:activity-functions options)
                            :context            (:context options)
                            :services           (:services options)})
  )

(defn create-service
  "Config can contain:
  :execute (default true): true if actions should be executed on transition.
  :deferEvents (default true): true if you want to defer send events until the start function is called.
  If false, an error will be thrown.
  :logger(default println) The logger used for log(...) actions"
  ([the-machine config]
   {:machine         the-machine
    :config          (merge {:deferEvents true
                             :execute     true
                             :logger      println} config)
    :on-transition   []
    :on-done         []
    :on-error        []
    :parent-callback []
    :state           (atom nil)
    :queue           (queue/create-queue)                   ; the event queue
    :cancel-channels (atom {})                              ;delayed events cancel channels
    :child-services  (atom {})
    })
  ([the-machine]
   (create-service the-machine {})))
(defn with-logger
  "Function to print log messages."
  [service log-fn]
  (assoc-in service [:config :logger] log-fn))
(defn with-on-done
  "Add an on-done event listener.
  Expects a function taken the on-done event."
  [service on-done-callback]
  (update service :on-done conj on-done-callback))

(defn with-on-error
  "Add an on-error event listener.
  Expects a function taken the on-error event."
  [service on-error-callback]
  (update service :on-error conj on-error-callback))

(defn with-parent [service parent-callback]
  (update service :parent-callback conj parent-callback))

(defn with-on-transition [service on-trans-fn]
  (update service :on-transition conj on-trans-fn))

(defn execute [service]
  (dispatch service :glas-state/execute-actions))


(defn start
  ([{:keys [machine] :as service}]
   (start service (sl/start-machine machine)))
  ([{:keys [state] :as service} initial-state]
   (reset! state initial-state)
   (queue/start-queue service)
   (execute service)))

(defn stop [service]
  (queue/stop-queue service)
  service)

(defn assign [context-update-fn]
  "Creates an event that will assign a new context value using the context-update-fn.
   You should only use this in the machine definition.
   The context function will be passed: the current context, the event.
   If the argument is not a function, than the function will be looked up in the
   machine config actions"
  {:type     :glas-state/assign-context
   :assigner context-update-fn})


(def send-event sl/send-event)

(defn forward
  "An action that forward the initiating event to the service corresponding to the supplied service-id"
  [service-id]
  (send-event (fn [c e] e) {:to service-id}))

(defn spawn
  ([src options]
   {:spawn src
    :options options})
  ([src]
   (spawn src {})))

(defn send-parent-event
  "Creates an event that will send a new event to the parent of the current machine.
  You should only use this in the machine definition.
  The function expects an event or function returning an event as first argument"
  [event]
  {:type  :glas-state/send-parent
   :event event})

(defn log [fn-or-msg]
  {:type      :glas-state/log
   :fn-or-msg fn-or-msg})


(defn- resolve-action-keyword [{:keys [config machine] :as service} action]
  (if (keyword? action)
    (let [resolved (get-in machine [:actions action])]
      (when-not resolved
        (utils/service-log-warn service (str "Unresolved action " action " in machine " (:id machine))))
      resolved)
    action))

(defn- execute-log-action [{:keys [config] :as _service} {:keys [fn-or-msg] :as _fn-or-msg} event context]
  (let [msg (if (fn? fn-or-msg)
              (fn-or-msg context event)
              fn-or-msg)]
    ((:logger config) msg)
    context))

(defn- execute-assign-action [{:keys [] :as service} action event context]
  (let [fn-or-value (resolve-action-keyword service (:assigner action))]
    (if (fn? fn-or-value)
      (fn-or-value context event)
      fn-or-value)))
(defn- execute-send-action [service {:keys [event options] :as _action} org-event context]
  (let [new-event event
        new-event (if (fn? new-event) (new-event context org-event) new-event)
        options options]
    (dispatch service new-event (or options {}))
    context))


(defn- execute-send-parent-action [service {:keys [event] :as _action} org-event context]
  (let [new-event event
        new-event (if (fn? new-event) (new-event context org-event) new-event)
        parent-callback (:parent-callback service)]
    (doseq [p parent-callback]
      (p new-event))
    context))

(defn- create-invoke-child-fn [service id src data invoke-event]
  (cond
    (map? src)
    (fn [callback on-event]
      ;      (utils/todo "not working")
      (let [child-machine (machine-options src {:context data})
            child-callback (fn [event]
                             (callback event))
            child-on-done (fn [event]
                            (dispatch service {:type (keyword "done.invoke" (str "child." (name id)))
                                               :data (:data event)}))
            child-on-error (fn [event]
                             (dispatch service {:type (keyword "error.invoke" (str "child." (name id)))
                                                :data (:data event)}))
            child-service (-> (create-service child-machine)
                              (with-parent child-callback)
                              (with-on-done child-on-done)
                              (with-on-error child-on-error)
                              (start))]
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


(defn- invoke-child [service context action event]
  (let [invoke (:config action)

        src (:src invoke)
        src (if (keyword? src)
              (get-in service [:machine :services src])
              src)
        _ (assert (not (nil? src)) (str "src is nil for  " (:src invoke)))
        id (or (:id invoke) (create-invoke-child-id src))
        _ (assert id (str "invoke service need's an id: " invoke))
        ;_
        ;_ (assert (fn? src) "Invoke src must be a function")
        data (:data invoke)
        data (if (fn? data)
               (data context event)
               data)
        invoke-fn (create-invoke-child-fn service id src data action)
        callback (fn [event]
                   (cond
                     (= (sl/event-type event) :done/.)
                     (let [new-event {:type (keyword "done.invoke" (str "child." (name id)))
                                      :data (:data event)}]
                       (dispatch service new-event))

                     (= (sl/event-type event) :error/.)
                     (let [new-event {:type (keyword "error.invoke" (str "child." (name id)))
                                      :data (:data event)}]
                       (dispatch service new-event))
                     :else
                     (dispatch service event))
                   )
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


(defn- execute-internal-action [{:keys [machine] :as service} action event context]
  (case (:type action)
    :glas-state/execute-actions context
    :glas-state/log (execute-log-action service action event context)
    :glas-state/assign-context (execute-assign-action service action event context)
    :glas-state/send (execute-send-action service action event context)
    :glas-state/send-parent (execute-send-parent-action service action event context)
    :glas-state/invoke (do (invoke-child service context action event)
                           context)
    :glas-state/invoke-cleanup (do (invoke-child-cleanup service context action)
                                   context)
    :glas-state/start-activities (do (start-activities service context action)
                                     context)
    :glas-state/stop-activities (do (stop-activities service context action)
                                    context)
    (throw
      (ex-info "Unknown internal action " {:action  action
                                           :event   event
                                           :context context
                                           :machine (:id machine)}))))



(defn- execute-action [{:keys [machine] :as service} action event context]
  (let [action (resolve-action-keyword service action)]
    (try
      (cond
        (nil? action)
        context

        (fn? action)
        (do
          (action context event)
          context)

        (map? action)
        (if-let [exec (:exec action)]
          (exec context event)
          (execute-internal-action service action event context)))

      (catch #?(:clj  Exception
                :cljs :default) e
        (throw (ex-info "Exception while executing statechart action!" {:machine-id (:id machine)
                                                                        :action     action
                                                                        :event      event
                                                                        :context    context} e))))
    )
  )
(defn- execute-all-actions [{:keys [state] :as service} context]
  #_(println "executing " (:actions @state))
  (let [new-context (reduce (fn [context action]
                              (execute-action service action (:event @state) context)) context (:actions @state))]
    (swap! state (fn [s]
                   (-> s
                       (assoc :context new-context)
                       (assoc :actions []))))))


(defn- execute-event-later [{:keys [cancel-channels] :as service} event {:keys [delay id] :as event-data}]
  (let [id (or id event)
        cancel-chan (ca/chan 1)]
    (swap! cancel-channels conj [id cancel-chan])
    (ca/go
      (let [[v _] (ca/alts! [(ca/timeout delay) cancel-chan])]
        (ca/close! cancel-chan)
        (swap! cancel-channels dissoc id)
        (when (not v)
          (dispatch service event (dissoc event-data :delay))
          )))))

(defn- execute-child-event [service event event-data]
  (dispatch-to-child-service service (:to event-data) event)
  )

(defn- normalize-event [event]
  (if (keyword? event)
    {:type event}
    event))

(defn- resolve-invoke-data [event context]
  (update event :data (fn [d]
                        (if (fn? d)
                          (d event context)
                          d))))

(defn- execute-event [{:keys [state machine on-done on-error on-transition] :as service} event {:keys [delay to] :as event-data}]
  (let [event (normalize-event event)]
    (fn []
      (cond
        (and delay (not= delay 0))
        (execute-event-later service event event-data)

        to
        (execute-child-event service event event-data)

        :else
        (if (= :done/. (sl/event-type event))
          (let [event (resolve-invoke-data event (:context state))]
            (doseq [on-done-callback on-done]
              (on-done-callback event)))
          (if (= :error/. (sl/event-type event))
            (let [event (resolve-invoke-data event (:context state))]
              (doseq [on-error-callback on-error]
                (on-error-callback event)))
            (let [trans-state (sl/transition-machine machine @state event)
                  prev-context (:context @state)
                  new-value (:value trans-state)
                  new-context (:value trans-state)
                  new-actions (:actions trans-state)
                  new-state (-> @state
                                (assoc :value new-value)
                                (assoc :context new-context)
                                (update :actions concat new-actions)
                                (assoc :event event)
                                (assoc :changed (or (not= (:value @state) new-value) (not= (:context @state) new-context)))
                                (assoc :handled (if (:not-handled trans-state)
                                                  false
                                                  true)))]
              (reset! state new-state)
              #_(when (:not-handled trans-state)
                  (utils/service-log-warn service (str "The event was not handled: " event)))
              (when (get-in service [:config :execute])
                (execute-all-actions service prev-context))
              (doseq [transition-listener on-transition]
                (transition-listener @state))
              )))
        )
      )))


(defn dispatch
  "Send an event with possible extra event data to a service"
  ([service event]
   (dispatch service event {}))
  ([service event event-data]
   (queue/service-put service (execute-event service event event-data))
   service))

(defn dispatch-nil
  "Send an event using dispatch but returns nil.
  Can be used for testing in the repl."
  ([service event]
   (dispatch service event {})
   nil)
  ([service event event-data]
   (dispatch service event event-data)
   nil))

(defn service-state [{:keys [state] :as _service}]
  "Return the full current state of this service"
  @state)
(defn service-value [{:keys [state] :as _service}]
  "Return the current value of this service"
  (:value @state))
(defn service-context [{:keys [state] :as _service}]
  "Return the current context of this service"
  (:context @state))

(defn service-fn [fun]
  "Create a child services using a function with arguments [data callback event]
  where:
  - data is the service  initial data
  - callback is the parent callback
  - event is the new invoked event"
  (fn [data event]
    (fn [callback on-event]
      (on-event (fn [e]
                  (ca/go (try
                           (fun data callback e)
                           (catch Exception e (callback {:type :error/.
                                                         :data {:exception e}}))))))
      nil)))

(defn service-call [fun]
  "Expects a function taking a data map and a callback-function.
  The function is directly async called once.
  It is up to the function to send a :done. event"
  (fn [data event]
    (fn [callback on-event]
      (ca/go (try (fun data callback)
                  (catch Exception e (callback {:type :error/.
                                                :data {:exception e}}))))
      nil)))

(defn service-call-return [fun]
  "Expects a function taking a data map argument.
   The function is directly async called once and
   the return value is send in the :data key of a :done/. event"
  (fn [data event]
    (fn [callback on-event]
      (ca/go (try (let [result (fun data)]
                    (callback {:type :done/. :data result}))
                  (catch Exception e (callback {:type :error/.
                                                :data {:exception e}}))))
      nil)))

(comment

  (def mape-machine {:id      :mape
                     :initial :idle
                     :states  {:idle    {:entry [(log "idle entry action")
                                                 (assign {:context-init true})
                                                 (send-parent-event {:type :machine-started})]
                                         :exit  :idle-exit-action
                                         :on    {:next  :started
                                                 :hello {:actions (log "Hello...")}}}
                               :started {:entry (assign (fn [c e] (merge c {:context-from-event e})))
                                         :on    {:stop :stopped}}
                               :stopped {:type :final}}
                     :actions {:idle-exit-action (fn [c e] (println "Action :idle-exit-action"))}})




  (def ping-pong-machine
    {:id      :ping-pong
     :initial :idle
     :on      {:stop :.done-with-it}
     :invoke  [{:id  :test
                :src (service-fn (fn [data callback event]
                                   (println "service event: " event)
                                   (callback {:type :service-event-done
                                              :data :hello})
                                   (println "service callback send")))}]
     :states  {:idle         {:entry (send-event :test-event {:to :test})
                              :on    {:start-ping-pong :ping}}
               :ping         {:entry [(log "ping ")
                                      (send-event :pong {:delay 1000})]
                              :on    {:pong :pong}}
               :pong         {:entry [(log "pong")
                                      (send-event :ping {:delay 1000})]
                              :on    {:ping :ping}}
               :done-with-it {:type  :final
                              :entry (log "DONE WITH PING-PONG")}}
     })


  (def s (-> (create-service ping-pong-machine)
             (with-on-transition (fn [state] (println "TRANS" state)))
             (with-on-done (fn [event] (println "GOT ON-DONE" event)))
             (with-parent (fn [event] (println "GOT PARENT EVENT " event)))
             ))

  #_(dispatch s :hello)
  (start s)
  (dispatch s :start-ping-pong)
  (dispatch s :stop)
  (stop s)


  (let [service (-> (create-service mape-machine)
                    (with-on-transition (fn [state] (println "TRANS" state)))
                    (with-on-done (fn [event] (println "GOT ON-DONE" event)))
                    (with-parent (fn [event] (println "GOT PARENT EVENT " event)))
                    )]
    (dispatch service :hello)
    (dispatch service :next)
    (dispatch service :stop)
    (start service)
    (stop service)
    nil)

  (def on-error-test-machine
    {:id      :on-error-test
     :initial :no-error
     :on {:test-exception :.test-exception}
     :states  {:no-error      {:invoke {:id      :test
                                        :src     (service-call (fn [data callback]
                                                                 (println "in service call" data)
                                                                 (callback {:type :done/.
                                                                            :data "this-is-done"})))
                                        :data    {:hello :peter}
                                        :on-done {:target  :to-error-test
                                                  :actions (fn [c e] (println "On-done-action:" e))}}}
               :to-error-test {:invoke {:id       :error-test
                                        :data     {:error-call-data :test}
                                        :src      (service-call (fn [data callback]
                                                                  (println "in error service call " data)
                                                                  (callback {:type :error/.
                                                                             :data "This is an error"})))
                                        :on-error {
                                                   :actions (fn [c e] (println "On-Error-action" e))}}}
               :test-exception {:invoke {:id :exception-test
                                        :data {:exception-test-call-data :test}
                                        :src (service-call (fn [data callback]
                                                             (println "in exception service call " data)
                                                             (throw (ex-info "This is an exception" {:info :test}))))
                                        :on-error {:actions (fn [c e] (println "Got exception action" e))}}}}})

  (let [service (-> (create-service on-error-test-machine)
                    (with-on-transition (fn [state] (println "TRANS" state)))
                    (with-on-done (fn [event] (println "GOT ON-DONE" event)))
                    (with-on-error (fn [event] (println "GOT ON-ERROR" event)))
                    (with-parent (fn [event] (println "GOT PARENT EVENT " event)))
                    )]
    (start service)
    (dispatch service :test-exception)
    #_(stop service)
    nil)
  )
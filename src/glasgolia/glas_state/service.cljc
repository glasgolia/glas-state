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

(defn- sync-action-handler [{:keys [machine send-channel] :as service} context action event meta]
  (try
    (if (keyword? action)
      (sync-action-handler service context (get (:actions machine) action) event meta)
      (do
        (cond
          (fn? action) (do (action context event meta) context)
          (map? action) (let [type (:type action)
                              exec (:exec action)]
                          (if exec
                            ;execute the function defined in-line in the machine
                            (exec context event meta)
                            (if (and (or (string? type) (keyword? type)) (= (namespace type) "glas-state"))
                              ;We have an internal action
                              (case type
                                :glas-state/assign-context
                                ((:assigner action) context event meta)
                                :glas-state/send (let [event (:event action)
                                                       delay-context (:delay-context action)]
                                                   (if delay-context
                                                     (as/go (as/>! @send-channel {:event         event
                                                                                  :delay-context delay-context}))
                                                     (as/go (as/>! @send-channel {:event event})))
                                                   context)
                                (ex-info "Unknown internal action" {:action action}))
                              context)))
          :else context)))
    (catch #?(:clj  Exception
              :cljs :default) e (ex-info "Exception while executing statechart action!" {:machine-id (:id machine)
                                                                                         :action     action
                                                                                         :event      event
                                                                                         :meta       meta} e))))

(defn notify-listeners [{:keys [change-listener]} prev-state new-state]
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
          (let [new-context (action-handler inst c next-action event {:action next-action
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
  (if delay-context
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
    (swap! storage (fn [prev-state]
                     (let [context (:context prev-state)
                           new-state (sl/transition-machine machine prev-state event)
                           new-context (send-actions inst context (:actions new-state) event (:value new-state))
                           new-state (-> new-state
                                         (dissoc :actions)
                                         (assoc :context new-context))]
                       (notify-listeners inst prev-state new-state)
                       new-state))))
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
  - :service-data user-data that needs to be linked to this service"
  ([the-machine {:keys [state-atom change-listener parent-callback service-id auto-start service-data] :as _config}]
   (when (and state-atom (nil? @state-atom)) (reset! state-atom {}))
   (let [
         result {:storage         (or state-atom (atom {}))
                 :machine         the-machine
                 :service-id      (or service-id (:id the-machine))
                 :service-state   (atom :idle)
                 :send-channel    (atom nil)
                 :change-listener change-listener
                 :parent-send     parent-callback
                 :delayed-events  (atom {})
                 :action-handler  sync-action-handler
                 :service-data    service-data}]
     (if (or (nil? auto-start) auto-start)
       (start result)
       result)
     ))
  ([the-machine]
   (create-service the-machine {})))

(defn dispatch
  "Dispatch an event to this service.
  If you want to delay the handling of the event an amount of time,
  you can add a delay context map containing {:delay <delay in ms> :id <delay-id, used to cancel if needed>}"
  ([{:keys [send-channel machine] :as _service} event]
   (if @send-channel
     (as/go (as/>! @send-channel {:event event}))
     (ex-info "Call start before dispatching events" {:machine-id (:id machine) :event event})))
  ([{:keys [send-channel machine]} event delay-context]
   (when (not @send-channel) (ex-info "Call start before dispatching events" {:machine-id (:id machine) :event event :delay delay-context}))
   (as/go (as/>! @send-channel {:event event :delay-context delay-context}))))


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

(defn service-value [{:keys [storage] :as _service}]
  "Return the current value of this service"
  (:value @storage))
(defn service-context [{:keys [storage] :as _service}]
  "Return the current context of this service"
  (:context @storage))

(comment
  (def delay-test-machine {:initial :red
                           :context {:count 0}
                           :states  {:red    {:on    {:timer [{:target :green
                                                               :cond   (fn [c e] (< (:count c 0) 2))}
                                                              :done]}
                                              :entry [(sl/assign (fn [c e m]
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
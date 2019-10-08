(ns glasgolia.glas-state.interpreter
  (:require [glasgolia.glas-state.stateless :as sl]
            [clojure.core.async :as as]))


(def interpreter-logger
  (fn [{:keys [prev new]}]
    (let [prev-value (:value prev)
          new-value (:value new)
          prev-context (:context prev)
          new-context (:context new)]
      (when (not= prev-value new-value)
        (println "VALUE: " new-value " <-- " prev-value))
      (when (not= prev-context new-context)
        (println "CONTEXT: " new-context " <-- " prev-context)))))
(defn atom-store [store-atom]
  (fn [{:keys [new]}]
    (reset! store-atom new)))

(defn sync-action-handler [{:keys [machine send-channel] :as inst} context action event meta]
  (let []
    (cond
      (fn? action) (do (action context event meta) context)
      (keyword? action) (let [external-actions (:actions machine)
                              resolved-action (or (get external-actions action)
                                                  (:catch-all-action machine))]
                          (resolved-action context event meta)
                          context)
      (map? action) (let [type (:type action)
                          exec (:exec action)]
                      (if exec
                        (exec context event meta)
                        (if (and (or (string? type) (keyword? type))(= (namespace type) "glas-state"))
                          ;We have an internal action
                          (case type
                            :glas-state/assign-context ((:assigner action) context event meta)
                            :glas-state/send (let [event (:event action)
                                                   delay-context (:delay-context action)]
                                               (if delay-context
                                                 (as/go (as/>! @send-channel {:event         event
                                                                             :delay-context delay-context}))
                                                 (as/go (as/>! @send-channel {:event event})))
                                               context)
                            (do
                              (println "Unknown internal action: " action)
                              context))
                          (let [external-actions (:actions machine)
                                catch-all-action (:catch-all-action external-actions)]
                            (catch-all-action context event meta)
                            context))))
      :else context)))
(defn interpreter
  ([the-machine state-atom parent-send]
   (assert (sl/machine? the-machine) "Not a statechart machine, please use the glas-state.stateless/machine function")
   (when (nil? @state-atom) (reset! state-atom {}))
   (let [result {:storage        state-atom
                 :machine        the-machine
                 :send-channel   (atom nil)
                 :listeners      (atom [])
                 :parent-send    (atom parent-send)
                 :delayed-events (atom {})
                 :action-handler sync-action-handler}]
     result))
  ([the-machine state-atom]
   (interpreter the-machine state-atom nil))
  ([the-machine]
   (interpreter the-machine (atom {}))))
(defn notify-listeners [{:keys [listeners]} prev-state new-state]
  (let [notify-data {:prev prev-state :new new-state}]
    (doseq [l @listeners]
      (l notify-data))))
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

(defn init-service [{:keys [storage machine] :as inst}]
  (swap! storage (fn [prev-state]
                   (let [{:keys [actions context value] :as state} (sl/start-machine machine)
                         new-context (send-actions inst context actions :glas-state/start-machine value)
                         new-state (-> state
                                       (dissoc :actions)
                                       (assoc :context new-context))]
                     (notify-listeners inst prev-state new-state)
                     new-state)))
  (:value @storage))

(defn handle-event [{:keys [storage machine send-channel] :as inst} {:keys [event waiting-channel delay-context]}]
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


(defn start [{:keys [send-channel] :as service}]
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
    service))
(defn reset [service] (init-service service))
(defn stop [{:keys [send-channel delayed-events]}]
  (swap! delayed-events (fn [events]
                                     (doseq [[_ chan] events]
                                       (as/close! chan))
                                     {}))
  (as/close! @send-channel)
  (reset! send-channel nil))

(defn add-change-listener [interpreter fun]
  (swap! (:listeners interpreter) (fn [old] (conj old fun)))
  interpreter)

(defn transition
  ([{:keys [send-channel]} event]
   (if @send-channel
     (as/go (as/>! @send-channel {:event event}))
     (throw (Exception. "Service is not started"))))
  ([{:keys [send-channel]} event delay-context]
   (when (not @send-channel) (throw (Exception. "Service is not started")))
   (as/go (as/>! @send-channel {:event event :delay-context delay-context}))))

(defn transition-wait [{:keys [send-channel]} event]
  (if @send-channel
    (let [wait-channel (as/chan 1)]
      (as/>!! @send-channel {:event           event
                             :waiting-channel wait-channel})
      (as/<!! wait-channel))
    (throw (Exception. "Service is not started"))))

(defn state-value [{:keys [storage]}]
  (:value @storage))

(comment
  (def delay-test-machine (sl/machine {:initial :red
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
                                                 :done   {:type :final}}}))

  (def inst (let [state (atom {})]
              (-> (interpreter delay-test-machine)
                  (add-change-listener interpreter-logger)
                  (add-change-listener (atom-store state))
                  (start))
              ))
  (transition inst :timer)
  (transition inst :timer {:delay 5000})
  (stop inst)
  (start inst)
  (prn @(:delayed-events inst))
  (state-value inst)
  (prn inst)
  (let [state (atom {})
        inst (-> (interpreter delay-test-machine)
                 (add-change-listener interpreter-logger)
                 (add-change-listener (atom-store state))
                 (start))]
    (println "started")
    (transition inst :timer)
    (Thread/sleep 2000)
    (println "done"))

  )
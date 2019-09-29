(ns glasgolia.glas-state.interpreter
  (:require [glasgolia.glas-state.stateless :as sl]))


(defn assign [context-update-fn]
  {:type     :glas-state/assign-context
   :assigner context-update-fn})

(defn create-atom-sc-storage [atom-storage]
  {:swap
   (fn [swap-fn]
     (swap! atom-storage swap-fn))
   :read
   (fn []
     @atom-storage)})
(defn sync-action-handler [{:keys [machine] :as inst} context action event meta]
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
                        (if (= (namespace type) "glas-state")
                          ;We have an internal action
                          (case type
                            :glas-state/assign-context ((:assigner action) context event meta)
                            (do
                              (println "Unknown internal action: " action)
                              context))
                          (let [external-actions (:actions machine)
                                catch-all-action (:catch-all-action external-actions)]
                            (catch-all-action context event meta)
                            context))))
      :else context)))



(defn interpreter [the-machine]
  {:storage        (create-atom-sc-storage (atom nil))
   :machine        the-machine
   :action-handler sync-action-handler})

(defn context [{:keys [storage]}]
  (:context ((:read storage))))

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

(defn start [{:keys [storage machine] :as inst}]
  ((:swap storage) (fn [_prev-state]
                     (let [{:keys [actions context value] :as state} (sl/start-machine machine)
                           new-context (send-actions inst context actions :glas-state/start-machine value)]
                       (-> state
                           (dissoc :actions)
                           (assoc :context new-context)))))
  (:value ((:read storage))))


(defn send-event [{:keys [storage machine] :as inst} event]
  ((:swap storage) (fn [current-state]
                     (let [context (:context current-state)
                           new-state (sl/transition-machine machine current-state event)
                           new-context (send-actions inst context (:actions new-state) event (:value new-state))]
                       (-> new-state
                           (dissoc :actions)
                           (assoc :context new-context)))))
  (:value ((:read storage))))

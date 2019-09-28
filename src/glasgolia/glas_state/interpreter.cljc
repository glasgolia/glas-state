(ns glasgolia.glas-state.interpreter
  (:require [glasgolia.glas-state.stateless :as sl]))


(defn create-atom-sc-storage [atom-storage]
  {:swap
         (fn [swap-fn]
           (swap! atom-storage swap-fn))
   :read
         (fn []
           @atom-storage)})

(defn interpreter [the-machine]
  {:storage (create-atom-sc-storage (atom nil))
   :machine the-machine})

(defn- send-actions [inst actions]
  (println "EXECUTING " actions))

(defn start [{:keys [storage machine] :as inst}]
  ((:swap storage) (fn[_prev-state]
                             (let [{:keys [actions context] :as state} (sl/start-machine machine)]
                               (send-actions inst actions)
                               (dissoc state :actions))))
  (:value ((:read storage))))


(defn send-event [{:keys [storage machine] :as inst} event]
  ((:swap storage) (fn[current-state]
                   (let [new-state (sl/transition-machine machine current-state event)]
                     (send-actions inst (:actions new-state))
                     (dissoc new-state :actions))))
  (:value ((:read storage))))

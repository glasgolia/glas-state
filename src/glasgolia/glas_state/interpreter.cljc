(ns glasgolia.glas-state.interpreter
  (:require [glasgolia.glas-state.stateless :as sl]))






(def mape-machine {:id      mape
                   :initial :idle
                   :states  {:idle    {:entry :idle-entry-action
                                       :exit  :idle-exit-action
                                       :on    {:next :started}}
                             :started {:on {:stop :stopped}}
                             :stopped {:type :final}}})

(defn interpret [{:keys [state] :as service}]
  (println "interpret " @state)
  service)

(defn create-service
  [the-machine]
  {:state (atom {})
   :machine the-machine})

(defn start
  ([{:keys [machine] :as service}]
   (start service (sl/start-machine machine)))
  ([{:keys [state] :as service} initial-state]
   (reset! state initial-state)
   (interpret service)))

(defn dispatch
  ([service event]
   (sl/))
([service event event-context]))

(def mape (create-service mape-machine))

(start mape)

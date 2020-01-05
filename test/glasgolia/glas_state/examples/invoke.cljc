(ns glasgolia.glas-state.examples.invoke
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.core :as gs]))


(def invoke-child-machine
  {:id      :child
   :initial :idle
   :states  {:idle    {:entry (gs/log (fn [c e] (str "Child got initial context from parent: " (:child-context c))))
                       :on    {:next :started}}
             :started {:entry (gs/log "Child in started state")
                       :on {:next :done}}
             :done    {:entry (gs/log "Child in done state")
                       :type :final
                       :data {:msg "The Child is done with it."}}}
   })

(def invoke-main-machine
  {:id       :main
   :initial  :idle
   :states   {:idle           {:on {:next :child-services}}
              :child-services {:on     {:next :done}
                               :invoke {:src     :child-service
                                        :data    {:child-context "Initial child-context"}
                                        :on-done {:actions (gs/log (fn [c e](str "Got On-done event from child:" e)))}}}
              :done           {:type :final
                               :data (fn [c e] {:msg "Main machine is done"})}
              }
   :services {:child-service invoke-child-machine}
   })

(def service (-> (gs/create-service invoke-main-machine)
                 (gs/with-on-transition (fn[s] (when (:changed s) (println s))))
                 (gs/with-on-done (fn[e] (println "ON-DONE " e)))
                 (gs/start)) )

(do
  (gs/dispatch service :next)
  (gs/dispatch service :next {:to :child})
  (gs/dispatch service :next {:to :child})
  (gs/dispatch service :next)
  nil)


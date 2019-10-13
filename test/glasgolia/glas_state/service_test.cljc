(ns glasgolia.glas-state.service-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.service :refer :all]))

(def the-machine
  {:initial :a
   :context {:first-name "Peter"
             :last-name  "Muys"}
   :entry   [:entry-a (sl/assign (fn [c e m]
                                   (-> c
                                       (assoc :we-where-in-a true)
                                       (dissoc :first-name)
                                       (dissoc :last-name))))]
   :states  {:a {:entry   :entry-a
                 :on      {:switch :b}
                 :initial :on
                 :states  {:on  {:entry (fn [c e m]
                                          (println "Light a is on"))
                                 :on    {:switch :off}}
                           :off {:entry (sl/assign (fn [c _ _]
                                                     (assoc c :the-a-light-was-off true)))}}}
             :b {:entry :entry-b
                 :on    {:switch :a}}}})


(deftest interpreter-test
  (testing create-service
    "Testing transitions"
    (let [state (atom {})
          inst (-> (create-service the-machine {:state-atom   state
                                             :change-listener service-logger})
                   (start))]
      (is (= @state
             {:value {:a :on} :context {:we-where-in-a true}}))
      (dispatch-and-wait inst :switch)
      (is (= @state
             {:value {:a :off} :context {:we-where-in-a true :the-a-light-was-off true}}))
      (dispatch-and-wait inst :switch)
      (is (= @state
             {:value :b :context {:we-where-in-a true :the-a-light-was-off true}}))

      (stop inst))))


#_(def delay-test-machine (sl/machine {:initial :red
                          :states  {:red    {:on    {:timer :green}
                                             :entry (sl/send-event :timer {:delay 1000 :id :to-green-timer})}
                                    :green  {:on {:timer :orange}}
                                    :orange {:on {:timer :red}}}}))

#_(deftest delayed-events-test
  (testing create-service
    "Testing delay send"
    (let [inst (-> (create-service delay-test-machine {:change-listener service-logger})
                   (start))]
      (dispatch inst :dummy)
      (is inst)))) ; TODO

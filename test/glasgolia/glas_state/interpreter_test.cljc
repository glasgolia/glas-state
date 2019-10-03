(ns glasgolia.glas-state.interpreter-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.interpreter :refer :all]))

(def the-machine (sl/machine
                   {:initial :a
                    :context {:first-name "Peter"
                              :last-name  "Muys"}
                    :entry   [:entry-a (assign (fn [c e m]
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
                                            :off {:entry (assign (fn [c _ _]
                                                                   (assoc c :the-a-light-was-off true)))}}}
                              :b {:entry :entry-b
                                  :on    {:switch :a}}}}))


(deftest interpreter-test
  (testing interpreter
    "Testing transitions"
    (let [state (atom {})
          inst (-> (interpreter the-machine)
                   (add-change-listener interpreter-logger)
                   (add-change-listener (atom-store state))
                   (start))]
      (is (= @state
             {:value {:a :on} :context {:we-where-in-a true}}))
      (send-event-wait inst :switch)
      (is (= @state
             {:value {:a :off} :context {:we-where-in-a true :the-a-light-was-off true}}))
      (send-event-wait inst :switch)
      (is (= @state
             {:value :b :context {:we-where-in-a true :the-a-light-was-off true}}))

      (close inst))))

(def delay-test-machine (sl/machine {:initial :red
                          :states  {:red    {:on    {:timer :green}
                                             :entry (send :timer {:delay 1000 :id :to-green-timer})}
                                    :green  {:on {:timer :orange}}
                                    :orange {:on {:timer :red}}}}))

(deftest delayed-events-test
  (testing interpreter
    "Testing delay send"
    (let [inst (-> (interpreter delay-test-machine)
                   (add-change-listener interpreter-logger)
                   (start))]
      (send-event inst :dummy)
      (is inst)))) ; TODO


(ns glasgolia.glas-state.stateless-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.stateless :refer :all]))


(deftest event-type-tests
  (testing "event-type handling"
    (is (= :kw/test (event-type :kw/test)))
    (is (= :blabla (event-type [:blabla :extra])))
    (is (= :the-type (event-type {:type :the-type :a :b})))
    (is (= "Peter" (event-type "Peter")))
    (is (= 1234 (event-type 1234)))
    (is (= "true" (event-type true)))
    ))


(deftest create-initial-transition-state-tests
  (testing "Initial for leaf state"
    (let [state-def {:entry :lets-enter
                     :on    [:on-first :on-second]}]
      (is (= {:actions [:lets-enter]}
             (create-initial-transition-state "" state-def)))))

  (testing "Initial for non-leaf state"
    (let [state-def {:initial :state-2
                     :states  {:state-1 {}
                               :state-2 {:entry [:a :b [:c]]
                                         :on    [:on-state2]}}}]
      (is (= {:value :state-2 :actions [:a :b :c]}
             (create-initial-transition-state "" state-def)))))
  (testing "Initial for hierarchy state"
    (let [state-def {:initial :state-2
                     :states  {:state-1 {}
                               :state-2 {:initial :state-2-2
                                         :entry   :entry-state-2
                                         :on      [:a :b]
                                         :states  {:state-2-1 {}
                                                   :state-2-2 {:entry :entry-state-2-2}
                                                   }}}}]
      (is (= {:value {:state-2 :state-2-2} :actions [:entry-state-2 :entry-state-2-2]}
             (create-initial-transition-state "" state-def)))))
  (testing "Initial for Parallel state"
    (let [state-def {:type   :parallel
                     :states {:bold      {:initial :off
                                          :states  {:on  {:entry [:entry-bold-on]}
                                                    :off {:type :final}}}
                              :underline {:initial :on
                                          :states  {:on  {:entry :entry-underline-on
                                                          :type :final}
                                                    :off {:entry :entry-underline-off}}}}}]
      (is (= {:value   {:bold :off :underline :on}
              :actions [:entry-underline-on :done/.]
              :done true
              }
             (create-initial-transition-state "" state-def))))))

(deftest create-machine-test
  (testing "Create Machine"
    (let [
          m-def {:initial :a
                 :context {:init-context "test"}
                 :states  {:a {}
                           :b {}}}
          options {:guards {:g-a :guard-a-value}}
          expected {:machine-def m-def
                    :guards      (:guards options)
                    :actions     nil
                    :activities  nil
                    :context     (:context m-def)
                    :catch-all-action default-catch-all-action}]
      (is (= expected (machine m-def options))))))
(deftest start-machine-test
  (testing "minimal machine"
    (let [machine-def {:initial :start
                       :context {:name "Peter"}
                       :states  {:start {:entry :on-entry-start
                                         :type :final}
                                 :stop  {}}}]
      (is (= {:value :start
              :actions [:on-entry-start :done/. ]
              :context {:name "Peter"}
              :done true}
             (start-machine (machine machine-def)))))))

(deftest find-valid-handler-test
  (testing "event handlers handling"
    (let []
      (is (= {:target :new-target} (find-valid-handler :new-target {} {} :event)))
      (is (nil? (find-valid-handler nil {} {} :event)))
      (is (= {:target :new-target} (find-valid-handler [:new-target] {} {} :event)))
      (is (= {:target :second :cond :check-second}
             (find-valid-handler
               [{:target :first :cond :check-first}
                {:target :second :cond :check-second}]
               {:check-first  (fn [context event] (= {} context))
                :check-second (fn [context event] (:take-this context))}
               {:take-this true}
               :the-event))))))

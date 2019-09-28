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
             (create-initial-transition-state state-def)))))

  (testing "Initial for non-leaf state"
    (let [state-def {:initial :state-2
                     :states  {:state-1 {}
                               :state-2 {:entry [:a :b [:c]]
                                         :on    [:on-state2]}}}]
      (is (= {:value :state-2 :actions [:a :b :c]}
             (create-initial-transition-state state-def)))))
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
             (create-initial-transition-state state-def)))))
  (testing "Initial for Parallel state"
    (let [state-def {:type   :parallel
                     :states {:bold      {:initial :off
                                          :states  {:on  {:entry [:entry-bold-on]}
                                                    :off {}}}
                              :underline {:initial :on
                                          :states  {:on  {:entry :entry-underline-on}
                                                    :off {:entry :entry-underline-off}}}}}]
      (is (= {:value   {:bold :off :underline :on}
              :actions [:entry-underline-on]}
             (create-initial-transition-state state-def))))))

(deftest create-machine-test
  (testing "Create Machine"
    (let [m-def {:initial :a
                 :context {:init-context "test"}
                 :states  {:a {}
                           :b {}}}
          options {:guards {:g-a :guard-a-value}}
          expected {:machine-def m-def
                    :guards      (:guards options)
                    :actions     nil
                    :activities  nil
                    :context     (:context m-def)}]
      (is (= expected (machine m-def options))))))
(deftest start-machine-test
  (testing "minimal machine"
    (let [machine-def {:initial :start
                       :context {:name "Peter"}
                       :states  {:start {:entry :on-entry-start}
                                 :stop  {}}}]
      (is (= {:value :start :actions [:on-entry-start] :context {:name "Peter"}}
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

(def test-machine-1
  (machine
    {:id      :test-machine-1
     :initial :a
     :context {:name  "Peter"
               :years 47}
     :states  {:a {:exit  :exit-a
                   :entry :enter-a
                   :on    {:switch :b
                           :to-d :d}}
               :b {:entry :enter-b
                   :exit  :exit-b
                   :on    {:switch :c}}
               :c {:entry   :enter-c
                   :exit    :exit-c
                   :on      {:switch :a}
                   :initial :c-1
                   :states  {:c-1 {:entry :entry-c1
                                   :exit  :exit-c1
                                   :on    {:switch :c-2}}
                             :c-2 {:entry :entry-c2
                                   :exit  :exit-c2
                                   }}}
               :d {:entry :entry-d
                   :exit :exit-d
                   :type :parallel
                   :on {:to-c :c}
                   :states {:p1 {:initial :on
                                 :entry :entry-p1
                                 :exit :exit-p1
                                 :states {:on {:on {:switch :off}}
                                          :off {:on {:switch :on}}}}
                            :p2 {:initial :off
                                 :entry :entry-p2
                                 :exit :exit-p2
                                 :states {:on {:entry :p2-on-entry
                                               :exit :p2-on-exit
                                               :on {:switch :off}}
                                          :off {:entry :p2-off-entry
                                                :exit :p2-off-exit
                                                :on {:switch :on}}}}}}}}
    {}))

(deftest transition-machine-test
  (testing "Transition tests"
    (let [state (start-machine test-machine-1)
          state (transition-machine test-machine-1 state :switch)
          _ (is (= {:value :b :actions [:exit-a :enter-b]} state))
          state (transition-machine test-machine-1 state :switch)
          _ (is (= {:value {:c :c-1} :actions [:exit-b :enter-c :entry-c1]} state))
          state (transition-machine test-machine-1 state :switch)
          _ (is (= {:value {:c :c-2} :actions [:exit-c1 :entry-c2]} state))
          state (transition-machine test-machine-1 state :switch)
          _ (is (= {:value :a :actions [:exit-c2 :exit-c :enter-a]} state))
          state (transition-machine test-machine-1 state :to-d)
          _ (is (= {:value {:d {:p1 :on
                                :p2 :off}}
                    :actions [:exit-a :entry-d :entry-p1 :entry-p2 :p2-off-entry]} state))
          state (transition-machine test-machine-1 state :switch)
          _ (is (= {:value {:d {:p1 :off
                                :p2 :on}}
                    :actions [:p2-off-exit :p2-on-entry]} state))
          state (transition-machine test-machine-1 state :to-c)
          _(is (= {:value {:c :c-1} :actions [:p2-on-exit :exit-p1 :exit-p2 :exit-d :enter-c :entry-c1]} state))
          ])))
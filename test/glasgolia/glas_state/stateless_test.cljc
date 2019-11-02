(ns glasgolia.glas-state.stateless-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.stateless :refer :all]))

(defmacro validate [{exp-value   :value
                     exp-actions :actions
                     exp-context :context}
                    state]
  `(do
     (is (= ~exp-value (:value ~state)) "Values are not equal")
     (is (= (into #{} ~exp-actions) (into #{} (:actions ~state))) "Actions are not equal")
     (is (= ~exp-context (:context ~state)) "Context are not equal")
     nil))

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

  (testing "Initial for branch state"
    (let [state-def {:initial :state-2
                     :states  {:state-1 {}
                               :state-2 {:entry [:a :b [:c]]
                                         :on    [:on-state2]}}}]
      (is (= {:value :state-2 :actions [:a :b :c] :on-done-events []}
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
      (is (= {:value {:state-2 :state-2-2} :actions [:entry-state-2 :entry-state-2-2] :on-done-events []}
             (create-initial-transition-state "" state-def)))))
  (testing "Initial for Parallel state"
    (let [state-def {:type   :parallel
                     :states {:bold      {:initial :off
                                          :states  {:on  {:entry [:entry-bold-on]}
                                                    :off {}}}
                              :underline {:initial :on
                                          :states  {:on  {:entry :entry-underline-on
                                                          }
                                                    :off {:entry :entry-underline-off}}}}}]
      (is (= {:value          {:bold :off :underline :on}
              :actions        [:entry-underline-on]
              :on-done-events []
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
          expected {:initial    :a
                    :context    {:init-context "test"}
                    :states     {:a {}
                                 :b {}}
                    :guards     (:guards options)
                    :actions    nil
                    :activities nil
                    :services   nil}]
      (is (= expected (machine-options m-def options))))))
(deftest start-machine-test
  (testing "minimal machine"
    (let [machine-def {:initial :start
                       :context {:name "Peter"}
                       :states  {:start {:entry :on-entry-start}
                                 :stop  {}}}]
      (is (= {:value   :start
              :actions [:on-entry-start]
              :context {:name "Peter"}
              }
             (start-machine machine-def))))))



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
  {:id      :test-machine-1
   :initial :a
   :context {:name  "Peter"
             :years 47}
   :states  {:a {:exit  :exit-a
                 :entry :enter-a
                 :on    {:switch :b
                         :to-d   :d}}
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
             :d {:entry  :entry-d
                 :exit   :exit-d
                 :type   :parallel
                 :on     {:to-c :c}
                 :states {:p1 {:initial :on
                               :entry   :entry-p1
                               :exit    :exit-p1
                               :states  {:on  {:on {:switch :off}}
                                         :off {:on {:switch :on}}}}
                          :p2 {:initial :off
                               :entry   :entry-p2
                               :exit    :exit-p2
                               :states  {:on  {:entry :p2-on-entry
                                               :exit  :p2-on-exit
                                               :on    {:switch :off}}
                                         :off {:entry :p2-off-entry
                                               :exit  :p2-off-exit
                                               :on    {:switch :on}}}}}}}})

(deftest transition-machine-test
  (testing "Transition tests"
    (let [state (start-machine test-machine-1)
          state (transition-machine test-machine-1 state :switch)
          _ (validate {:value   :b
                       :actions [:exit-a :enter-b]}
                      state)
          state (transition-machine test-machine-1 state :switch)
          _ (validate {:value {:c :c-1} :actions [:exit-b :enter-c :entry-c1]} state)
          state (transition-machine test-machine-1 state :switch)
          _ (validate {:value {:c :c-2} :actions [:exit-c1 :entry-c2]} state)
          state (transition-machine test-machine-1 state :switch)
          _ (validate {:value :a :actions [:exit-c2 :exit-c :enter-a]} state)
          state (transition-machine test-machine-1 state :to-d)
          _ (validate {:value   {:d {:p1 :on
                                     :p2 :off}}
                       :actions [:exit-a :entry-d :entry-p1 :entry-p2 :p2-off-entry]} state)
          state (transition-machine test-machine-1 state :switch)
          _ (validate {:value   {:d {:p1 :off
                                     :p2 :on}}
                       :actions [:p2-off-exit :p2-on-entry]} state)
          state (transition-machine test-machine-1 state :to-c)
          _ (validate {:value {:c :c-1} :actions [:p2-on-exit :exit-p1 :exit-p2 :exit-d :enter-c :entry-c1]} state)
          state (transition-machine test-machine-1 state :non-existing-event)
          _ (validate {:value {:c :c-1} :actions []} state)
          ])))


;(defn on-off-switch [initial switch-event]
;  {:initial initial
;   {:states {:off {:on {switch-event :on}}
;             :on  {:on {switch-event :off}}}}})
;(defn bold-on-off-switch (on-off-switch  :off :bold-switch))
;(defn underline-on-off-switch (on-off-switch :off :underline-switch))

;(def color-def {:initial :color-black
;                {:states {:color-black {:on {:color-switch :color-red}}
;                          :color-red   {:on {:color-switch :color-green}}
;                          :color-green {:on {:color-switch :color-black}}}}})
;



(deftest ^:test-refresh/focus_not  on-done-init-test
  (testing "Testing on-done initial states"
    (let [state (start-machine
                  {:initial :a
                   :states  {:a {:initial :a1
                                 :states  {:a1 {:initial :a12
                                                :states  {:a11 {}
                                                          :a12 {:type :final}}}
                                           :a2 {}}}
                             :b {}
                             }})]
      (is (= {:value   {:a {:a1 :a12}}
              :actions [{:type  :glas-state/send
                         :event :done/..a.a1}]
              :context nil} state)))
    (let [state (start-machine

                  {:initial :a
                   :states  {:a {:type :final}
                             :b {}
                             }})]
      (is (= {:value   :a
              :actions [(done-event "")]
              :context nil} state)))
    (let [state (start-machine
                  {:initial :a
                   :states  {:a {:initial :p
                                 :states  {:p {:type   :parallel
                                               :states {:p1 {}
                                                        :p2 {:type :final}
                                                        }}}}}})]
      (is (= {:value   {:a {:p {:p1 nil :p2 nil}}}
              :actions [] :context nil} state))
      )
    (let [state (start-machine
                  {:initial :a
                   :states  {:a {:initial :p
                                 :states  {:p {:type   :parallel
                                               :states {:p1 {:type :final}
                                                        :p2 {:type :final}
                                                        }}}}}})]
      (is (= {:value   {:a {:p {:p1 nil :p2 nil}}}
              :actions [(done-event ".a.p")] :context nil} state))
      )
    )
  )

(deftest ^:test-refresh/focus-not relative-target-test
  (let [machine
        {:initial :c
         :on      {:go-to-b {:target  :.b
                             :actions [:test]}}
         :states  {:a {}
                   :b {}
                   :c {:initial :c1
                       :entry   :entry-c
                       :exit    :exit-c
                       :on      {:c2 {:target :.c2 :actions :goto-c2-action}
                                 :c1 :.c1}
                       :states  {:c1 {:entry :entry-c1
                                      :exit  :exit-c1}
                                 :c2 {}}}
                   }
         }
        state (start-machine machine)
        _ (validate {:value {:c :c1} :actions [:entry-c1 :entry-c]} state)
        state (transition-machine machine state :c2)
        _ (validate {:value {:c :c2} :actions [:exit-c1 :goto-c2-action]} state)
        state (transition-machine machine state :go-to-b)
        _ (validate {:value :b :actions [:exit-c :test]} state)
        ]))
;(def example-cmp-machine
;  {:id      :examples-component
;   :initial :please-select-example
;   :states
;            {:please-select-example {:on
;                                     {:select-example
;                                      {:target  :show-example
;                                       :actions (assign (fn [c e m]
;                                                         (println "......." e)
;                                                         (assoc c :selected (:example-id e))))}}}
;             :show-example {}}
;   :context {:list  []
;             :selected nil}})
;
;(deftest ^:test-refresh/focus on-with-actions-test
;  (let[state (start-machine example-cmp-machine)
;       _  (validate {:value :please-select-example :context {:list[] :selected nil}} state)
;       state (transition-machine example-cmp-machine state {:type :select-example :example-id :the-selected-id})
;       _  (validate {:value :show-example :context {:list[] :selected :the-selected-id}} state)
;
;       ]))

(deftest ^:test-refresh/focus_not invoke-test)

(deftest ^:test-refresh/focus_not on-done-transition-test
  (let [the-machine
        {:initial :a
         :states  {:a {:on {:switch :b}}
                   :b {:type :final}}}
        state (start-machine the-machine)
        _ (validate {:value :a :actions [] :context nil} state)
        state (transition-machine the-machine state :switch)
        _ (validate {:value   :b
                     :actions [(done-event "")]} state)
        ]
    )
  (let [the-machine
        {:initial :a
         :states  {:a {:initial :a1
                       :states  {:a1 {:on {:switch :a2}}
                                 :a2 {:type :final}}
                       :b       {:type :final}}}
         }
        state (start-machine the-machine)
        _ (validate {:value {:a :a1} :actions [] :context nil} state)
        state (transition-machine the-machine state :switch)
        _ (validate {:value   {:a :a2}
                     :actions [(done-event ".a")]} state)
        ]
    )
  (let [the-machine
        {:initial :a
         :states  {:a

                   {:initial :p
                    :states  {:p
                              {
                               :type   :parallel
                               :states {:bold  {:initial :off
                                                :states  {:on        {:on {:switch-bold :off
                                                                           :bold-finish :bold-done}}
                                                          :off       {:on {:switch-bold :on
                                                                           :bold-finish :bold-done}}
                                                          :bold-done {:type :final}
                                                          }}
                                        :color {:initial :black
                                                :states  {:black      {:on {:switch-color :red
                                                                            :color-finish :color-done}}
                                                          :red        {:on {:switch-color :black
                                                                            :color-finish :color-done}}
                                                          :color-done {:type :final}}}}}}}}
         }
        state (start-machine the-machine)
        _ (validate {:value {:a {:p {:bold :off :color :black}}} :actions [] :context nil} state)
        state (transition-machine the-machine state :bold-finish)
        _ (validate {:value {:a {:p {:bold :bold-done :color :black}}} :actions []} state)
        state (transition-machine the-machine state :color-finish)
        _ (validate {:value {:a {:p {:bold :bold-done :color :color-done}}} :actions [(done-event ".a.p")]} state)
        ]
    )
  )

(deftest ^:test-refresh/focus-not invoke-test
  (let [the-machine
        {:initial :a
         :states  {:a    {:invoke {:id :invoke-in-a}
                          :on     {:next :b}}
                   :b    {:initial :b1
                          :on      {:next :c}
                          :invoke  {:id :invoke-in-b}
                          :states  {:b1 {:invoke {:id :invoke-in-b1}
                                         }}}
                   :c    {:type   :parallel
                          :states {:c1 {:invoke {:id :invoke-in-c1}}
                                   :c2 {:invoke {:id :invoke-in-c2}}}}
                   :done {:type :final}}}
        state (start-machine the-machine)
        _ (validate {:value :a :actions [{:type :glas-state/invoke :config {:id :invoke-in-a}}] :context nil} state)
        state (transition-machine the-machine state :next)
        _ (validate {:value {:b :b1} :actions [{:type :glas-state/invoke-cleanup :id :invoke-in-a}
                                               {:type :glas-state/invoke :config {:id :invoke-in-b1}}
                                               {:type :glas-state/invoke :config {:id :invoke-in-b}}] :context nil} state)
        state (transition-machine the-machine state :next)
        _ (validate {:value {:c {:c1 nil
                                 :c2 nil}} :actions [{:type :glas-state/invoke-cleanup :id :invoke-in-b1}
                                                     {:type :glas-state/invoke-cleanup :id :invoke-in-b}
                                                     {:type :glas-state/invoke :config {:id :invoke-in-c1}}
                                                     {:type :glas-state/invoke :config {:id :invoke-in-c2}}] :context nil} state)
        ]
    )
  )

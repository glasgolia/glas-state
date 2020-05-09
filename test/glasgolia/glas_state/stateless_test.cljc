(ns glasgolia.glas-state.stateless-test
  (:require [#?(:clj clojure.test
                :cljs cljs.test) :as t]
            [glasgolia.glas-state.stateless :as sl]))

(defmacro validate [{exp-value   :value
                     exp-actions :actions
                     exp-context :context}
                    state]
  `(do
     (t/is (= ~exp-value (:value ~state)) "Values are not equal")
     (t/is (= (into #{} ~exp-actions) (into #{} (:actions ~state))) "Actions are not equal")
     (t/is (= ~exp-context (:context ~state)) "Context are not equal")
     nil))

(t/deftest event-type-tests
  (t/testing "event-type handling"
    (t/is (= :kw/test (sl/event-type :kw/test)))
    (t/is (= :blabla (sl/event-type [:blabla :extra])))
    (t/is (= :the-type (sl/event-type {:type :the-type :a :b})))
    (t/is (= "Peter" (sl/event-type "Peter")))
    (t/is (= 1234 (sl/event-type 1234)))
    (t/is (= "true" (sl/event-type true)))
    ))


(t/deftest create-initial-transition-state-tests
  (t/testing "Initial for leaf state"
    (let [state-def {:entry :lets-enter
                     :on    [:on-first :on-second]}]
      (t/is (= {:actions [:lets-enter]}
             (sl/create-initial-transition-state "" state-def)))))

  (t/testing "Initial for branch state"
    (let [state-def {:initial :state-2
                     :states  {:state-1 {}
                               :state-2 {:entry [:a :b [:c]]
                                         :on    [:on-state2]}}}]
      (t/is (= {:value :state-2 :actions [:a :b :c] :on-done-events []}
             (sl/create-initial-transition-state "" state-def)))))
  (t/testing "Initial for hierarchy state"
    (let [state-def {:initial :state-2
                     :states  {:state-1 {}
                               :state-2 {:initial :state-2-2
                                         :entry   :entry-state-2
                                         :on      [:a :b]
                                         :states  {:state-2-1 {}
                                                   :state-2-2 {:entry :entry-state-2-2}
                                                   }}}}]
      (t/is (= {:value {:state-2 :state-2-2} :actions [:entry-state-2 :entry-state-2-2] :on-done-events []}
             (sl/create-initial-transition-state "" state-def)))))
  (t/testing "Initial for Parallel state"
    (let [state-def {:type   :parallel
                     :states {:bold      {:initial :off
                                          :states  {:on  {:entry [:entry-bold-on]}
                                                    :off {}}}
                              :underline {:initial :on
                                          :states  {:on  {:entry :entry-underline-on
                                                          }
                                                    :off {:entry :entry-underline-off}}}}}]
      (t/is (= {:value          {:bold :off :underline :on}
              :actions        [:entry-underline-on]
              :on-done-events []
              }
             (sl/create-initial-transition-state "" state-def))))))


(t/deftest start-machine-test
  (t/testing "minimal machine"
    (let [machine-def {:initial :start
                       :context {:name "Peter"}
                       :states  {:start {:entry :on-entry-start}
                                 :stop  {}}}]
      (t/is (= {:value   :start
              :actions [:on-entry-start]
              :context {:name "Peter"}
              }
             (sl/start-machine machine-def))))))



(t/deftest find-valid-handler-test
  (t/testing "event handlers handling"
    (let []
      (t/is (= {:target :new-target} (sl/find-valid-handler :new-target {} {} :event)))
      (t/is (nil? (sl/find-valid-handler nil {} {} :event)))
      (t/is (= {:target :new-target} (sl/find-valid-handler [:new-target] {} {} :event)))
      (t/is (= {:target :second :cond :check-second}
             (sl/find-valid-handler
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

(t/deftest transition-machine-test
  (t/testing "Transition tests"
    (let [state (sl/start-machine test-machine-1)
          state (sl/transition-machine test-machine-1 state :switch)
          _ (validate {:value   :b
                       :actions [:exit-a :enter-b]}
                      state)
          state (sl/transition-machine test-machine-1 state :switch)
          _ (validate {:value {:c :c-1} :actions [:exit-b :enter-c :entry-c1]} state)
          state (sl/transition-machine test-machine-1 state :switch)
          _ (validate {:value {:c :c-2} :actions [:exit-c1 :entry-c2]} state)
          state (sl/transition-machine test-machine-1 state :switch)
          _ (validate {:value :a :actions [:exit-c2 :exit-c :enter-a]} state)
          state (sl/transition-machine test-machine-1 state :to-d)
          _ (validate {:value   {:d {:p1 :on
                                     :p2 :off}}
                       :actions [:exit-a :entry-d :entry-p1 :entry-p2 :p2-off-entry]} state)
          state (sl/transition-machine test-machine-1 state :switch)
          _ (validate {:value   {:d {:p1 :off
                                     :p2 :on}}
                       :actions [:p2-off-exit :p2-on-entry]} state)
          state (sl/transition-machine test-machine-1 state :to-c)
          _ (validate {:value {:c :c-1} :actions [:p2-on-exit :exit-p1 :exit-p2 :exit-d :enter-c :entry-c1]} state)
          state (sl/transition-machine test-machine-1 state :non-existing-event)
          _ (validate {:value {:c :c-1} :actions []} state)
          ])))






(t/deftest ^:test-refresh/focus_not  on-done-init-test
  (t/testing "Testing on-done initial states"
    (let [state (sl/start-machine
                  {:initial :a
                   :states  {:a {:initial :a1
                                 :states  {:a1 {:initial :a12
                                                :states  {:a11 {}
                                                          :a12 {:type :final}}}
                                           :a2 {}}}
                             :b {}
                             }})]
      (t/is (= {:value   {:a {:a1 :a12}}
              :actions [{:type  :glas-state/send
                         :event {:type :done/.a.a1 :data nil}}]
              :context nil} state)))
    (let [state (sl/start-machine

                  {:initial :a
                   :states  {:a {:type :final}
                             :b {}
                             }})]
      (t/is (= {:value   :a
              :actions [{:type  :glas-state/send
                         :event {:type :done/.
                                 :data nil}}
                        ]
              :context nil} state)))
    (let [state (sl/start-machine
                  {:initial :a
                   :states  {:a {:initial :p
                                 :states  {:p {:type   :parallel
                                               :states {:p1 {}
                                                        :p2 {:type :final}
                                                        }}}}}})]
      (t/is (= {:value   {:a {:p {:p1 nil :p2 nil}}}
              :actions [] :context nil} state))
      )
    (let [state (sl/start-machine
                  {:initial :a
                   :states  {:a {:initial :p
                                 :states  {:p {:type   :parallel
                                               :states {:p1 {:type :final}
                                                        :p2 {:type :final}
                                                        }}}}}})]
      (t/is (= {:value   {:a {:p {:p1 nil :p2 nil}}}
              :actions [(sl/done-event ".a.p" nil)] :context nil} state))
      )
    )
  )

(t/deftest ^:test-refresh/focus-not relative-target-test
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
        state (sl/start-machine machine)
        _ (validate {:value {:c :c1} :actions [:entry-c1 :entry-c]} state)
        state (sl/transition-machine machine state :c2)
        _ (validate {:value {:c :c2} :actions [:exit-c1 :goto-c2-action]} state)
        state (sl/transition-machine machine state :go-to-b)
        _ (validate {:value :b :actions [:exit-c :test]} state)
        ]))


(t/deftest ^:test-refresh/focus_not invoke-test)

(t/deftest ^:test-refresh/focus_not on-done-transition-test
  (let [the-machine
        {:initial :a
         :states  {:a {:on {:switch :b}}
                   :b {:type :final}}}
        state (sl/start-machine the-machine)
        _ (validate {:value :a :actions [] :context nil} state)
        state (sl/transition-machine the-machine state :switch)
        _ (validate {:value   :b
                     :actions [(sl/done-event "" nil)]} state)
        ]
    )
  (let [the-machine
        {:initial :a
         :states  {:a {:initial :a1
                       :states  {:a1 {:on {:switch :a2}}
                                 :a2 {:type :final}}
                       :b       {:type :final}}}
         }
        state (sl/start-machine the-machine)
        _ (validate {:value {:a :a1} :actions [] :context nil} state)
        state (sl/transition-machine the-machine state :switch)
        _ (validate {:value   {:a :a2}
                     :actions [(sl/done-event ".a" nil)]} state)
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
        state (sl/start-machine the-machine)
        _ (validate {:value {:a {:p {:bold :off :color :black}}} :actions [] :context nil} state)
        state (sl/transition-machine the-machine state :bold-finish)
        _ (validate {:value {:a {:p {:bold :bold-done :color :black}}} :actions []} state)
        state (sl/transition-machine the-machine state :color-finish)
        _ (validate {:value {:a {:p {:bold :bold-done :color :color-done}}} :actions [(sl/done-event ".a.p" nil)]} state)
        ]
    )
  )

;(deftest ^:test-refresh/focus-not done-events-tests
;  (let [the-machine {:initial :a
;                     :id :test
;                     :states {:a {:type :parallel
;                                  :states {:par1 {:initial :par11
;                                                  :states {:par11 {:on {:switch :par12}}
;                                                           :par12 {:type :final}}}
;                                           :par2 {:type :final}}}}}
;        state (sl/start-machine the-machine)
;        _ (validate {:value {:a {:par1 :par11 :par2 nil}} :context nil} state)
;        state (sl/transition-machine the-machine state :switch)
;        _ (validate {:value {:a {:par1 :par12 :par2 nil}} :actions [(sl/done-event ".a")] :context nil} state)
;        the-machine {:id :shopping
;                     :type :parallel
;                     :states {:user {:initial :pending
;                                     :states {:pending {:on {:resolve-user :success
;                                                             :reject-user :failure}}
;                                              :success {:type :final}
;                                              :failure {}
;                                              }}
;                              :items {:initial :pending
;                                      :states {:pending {:on {:resolve-items :success
;                                                              :reject-items :failure}}
;                                               :success {:type :final}
;                                               :failure {}}}}
;                     :on-done {:actions (assign (fn [c e] assoc c :done true))
;                               }}
;        state (sl/start-machine the-machine)
;        _ (validate {:value {:user :pending :items :pending} :context nil} state)
;        state (sl/transition-machine the-machine state :resolve-user)
;        _ (validate {:value {:user :success :items :pending} :context nil} state)
;        state (sl/transition-machine the-machine state :resolve-items)
;        _ (validate {:value {:user :success :items :success} :actions [(sl/done-event "")] :context nil} state)
;        the-machine {:id :test
;                     :initial :state-a
;                     :on-done {:actions :root-done}
;                     :states {
;                              :state-a {:initial :state-aa
;                                        :on {:go-b :state-b}
;                                        :on-done {:actions :a-done}
;                                        :states {:state-aa {:initial :state-aaa
;                                                            :on {:go-bb :state-bb}
;                                                            :on-done {:actions :aa-done}
;                                                            :states {:state-aaa {:on {:go-bbb :state-bbb}}
;                                                                     :state-bbb {:type :final}}}
;                                                 :state-bb {:type :final}}}
;                              :state-b {:type :final} }
;                     }
;        state (sl/start-machine the-machine)
;        _ (validate {:value {:state-a {:state-aa :state-aaa}}} state)
;        state (sl/transition-machine the-machine state :go-bbb)
;        _ (validate {:value {:state-a {:state-aa :state-bbb}} :actions [(sl/done-event ".state-a.state-aa")]} state)
;        state (sl/transition-machine the-machine state :done/.state-a.state-aa)
;        _ (validate {:value {:state-a {:state-aa :state-bbb}} :actions [:aa-done]} state)
;        ]))
(t/deftest ^:test-refresh/focus-not activitie-test
  (let[
       the-machine {:id :activity-test
                    :initial :start
                    :states {:start {:activities [:act-1]
                                     :initial :sub1
                                     :on {:done :done}
                                     :states {:sub1 {:on {:next :sub2}}
                                              :sub2 {:on {:next :sub1}
                                                     :activities [:act-1 :act-2]}}}
                             :done {:type :final}}}
       state (sl/start-machine the-machine)
       _ (validate {:value {:start :sub1} :actions [(sl/create-activities-event [:act-1])]} state)
       state (sl/transition-machine the-machine state :next)
       _ (validate {:value {:start :sub2} :actions [(sl/create-activities-event [:act-1 :act-2])]} state)
       state (sl/transition-machine the-machine state :next)
       _ (validate {:value {:start :sub1} :actions [(sl/create-activities-cleanup-event[:act-1 :act-2])]} state)
       state (sl/transition-machine the-machine state :done)
       _ (validate {:value :done :actions [(sl/create-activities-cleanup-event[:act-1])(sl/done-event "." nil)]} state)
       ]))
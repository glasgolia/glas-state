(ns glasgolia.glas-state.service-test
  (:require [#?(:clj clojure.test
                :cljs cljs.test) :as t]
            [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.core :as gs]))

;(def the-machine
;  {:initial :a
;   :context {:first-name "Peter"
;             :last-name  "Muys"}
;   :entry   [:entry-a (assign (fn [c e]
;                                   (-> c
;                                       (assoc :we-where-in-a true)
;                                       (dissoc :first-name)
;                                       (dissoc :last-name))))]
;   :states  {:a {:entry   :entry-a
;                 :on      {:switch :b}
;                 :invoke  {:id  :child-service-start
;                            :src (fn [context event]
;                                   (let []
;                                     (fn [callback on-event]
;                                       (println "Child Service Invoked")
;                                       (println "Context=" context)
;                                       (println "event=" event)
;
;                                       (fn [] (println "Child Service Cleanup Invoked")))
;                                     ))
;                           :on-done {:target :b}
;                            }
;
;                 :initial :on
;                 :states  {:on  {:entry (fn [c e]
;                                          (println "Light a is on"))
;                                 :on    {:switch :off}}
;                           :off {:entry (assign :turn-light-off)}}}
;             :b {:entry :entry-b
;                 :on    {:switch :a}}}})
;
;
;(deftest ^:test-refresh/focus-not interpreter-test
;  (testing create-service
;    "Testing transitions"
;    (let [state (atom {})
;          inst (-> (create-service the-machine {:state-atom      state
;                                                :actions         {:turn-light-off (fn [c _]
;                                                                                    (assoc c :the-a-light-was-off true))}})
;                   #_(start))
;          ]
;      (is (= @state
;             {:value {:a :on} :context {:we-where-in-a true}}))
;      (dispatch inst :hello :child-service-start  )
;      (dispatch-and-wait inst :switch)
;      (is (= @state
;             {:value {:a :off} :context {:we-where-in-a true :the-a-light-was-off true}}))
;      (dispatch-and-wait inst :switch)
;      (is (= @state
;             {:value :b :context {:we-where-in-a true :the-a-light-was-off true}}))
;
;      (stop inst))))
;
;(def example-cmp-machine
;  {:id      :examples-component
;   :initial :please-select-example
;   :states
;            {:please-select-example {:on
;                                     {:select-example
;                                      {:target  :show-example
;                                       :actions (sl/assign (fn [c e]
;                                                             (assoc c :selected (:example-id e))))}}}
;             :show-example          {}}
;   :context {:list     []
;             :selected nil}})
;
;
;(deftest ^:test-refresh/focus_not reactions-test
;  (let [state (atom {})
;        reactions-result (atom {})
;        inst (-> (create-service {:id      test
;                                  :initial :idle
;                                  :states  {:idle  {:on {:next :start}}
;                                            :start {:on {:next :stop}}
;                                            :stop  {}}}
;                                 {:state-atom      state
;                                  :change-listener service-logger})
;                 (start))
;        remove-reactor1 (add-value-reaction inst (fn [old new]
;                                                   (println "GOT REACTION" old new)
;                                                   (reset! reactions-result {:old old :new new})))
;        remove-reactor2 (add-context-reaction inst (fn [old new]))
;        _ (println "VALUE:" (service-value inst))
;        _ (dispatch-and-wait inst :next)
;        _ (is (= @reactions-result {:old :idle :new :start}))
;        _ (remove-reactor1)
;        _ (dispatch-and-wait inst :next)
;        _ (is (= @reactions-result {:old :idle :new :start}))
;
;        ]))
;
;(deftest ^:test-refresh/focus-not on-with-actions-test
;
;  (let [state (atom {})
;        inst (-> (create-service example-cmp-machine {:state-atom      state
;                                                      :change-listener service-logger})
;                 #_(start))]
;    (dispatch-and-wait inst {:type :select-example :example-id "blabla"})
;    (is (= @state {:value :show-example :context {:list [] :selected "blabla"}}))))
;
;
;
;(deftest ^:test-refresh/focus_not child-machines
;  (let [inst (create-service {:id      :invoke-test
;                              :initial :a
;                              :states  {:a {:invoke [{:id :child-machine
;                                                       :src     {:id      :child-machine
;                                                                :entry   (fn [c e] (println "Child-machine entry..."))
;                                                                :initial :child-a
;                                                                :states  {:child-a    {:entry (fn [c e] (println "child-machine: in :child-a"))
;                                                                                       :on    {:next :child-done}}
;                                                                          :child-done {:type  :final
;                                                                                       :entry (fn [c e] (println "Child-machine in :child-done"))}}}
;                                                      :on-done {:target :b
;                                                                :actions (fn [c e] (println "Child-machine on-done -> send target b"))}}]}
;                                        :b {:entry (fn [c e] (println "Parent-machine entry :b"))
;                                            :type  :final}
;                                        }} {:change-listener service-logger})
;
;        _ (start inst)
;        _ (dispatch inst :next {:to :child-machine})
;        ;_ (dispatch inst :dummy)
;        _ (Thread/sleep 1000)
;        ])
;
;  )
;(deftest create-machine-test
;  (testing "Create Machine"
;    (let [
;          m-def {:initial :a
;                 :context {:init-context "test"}
;                 :states  {:a {}
;                           :b {}}}
;          options {:guards {:g-a :guard-a-value}}
;          expected {:initial    :a
;                    :context    {:init-context "test"}
;                    :states     {:a {}
;                                 :b {}}
;                    :guards     (:guards options)
;                    :actions    nil
;                    :activity-functions nil
;                    :services   nil}]
;      (is (= expected (machine-options m-def options))))))
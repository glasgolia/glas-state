(ns glasgolia.glas-state.core-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.core :refer :all]))

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
          state (transition-machine test-machine-1 state :non-existing-event)
          _(is (= {:value {:c :c-1} :actions []} state))
          ])))

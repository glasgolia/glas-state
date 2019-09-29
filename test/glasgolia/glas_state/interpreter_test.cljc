(ns glasgolia.glas-state.interpreter-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.interpreter :refer :all]
            [glasgolia.glas-state.stateless :refer :all]))

(def the-machine (machine
                   {:initial :a
                    :context {:first-name "Peter"
                             :last-name "Muys"}
                    :entry [:entry-a (assign (fn[c e m]
                                               (assoc c :we-were-in-a true)))]
                    :states {:a {:entry :entry-a
                                 :on {:switch :b}
                                 :initial :on
                                 :states {:on {:entry (fn [c e m]
                                                        (println "Light a is on"))
                                               :on {:switch :off}}
                                          :off {:entry (assign (fn[c _ _]
                                                                 (assoc c :the-a-light-was-off true)))}}}
                             :b {:entry :entry-b
                                 :on {:switch :a}}}}))

(deftest interpreter-test
  (testing "Atom based interpreter"
    (let [inst (interpreter the-machine)]
      (println (start inst) (context inst))
      (println (send-event inst :switch) (context inst))
      (println (send-event inst :switch) (context inst)))))
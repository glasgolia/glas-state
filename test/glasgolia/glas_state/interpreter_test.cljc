(ns glasgolia.glas-state.interpreter-test
  (:require [clojure.test :refer :all]
            [glasgolia.glas-state.interpreter :refer :all]
            [glasgolia.glas-state.stateless :refer :all]))

(def the-machine (machine
                   {:initial :a
                    :states {:a {:entry :entry-a
                                 :on {:switch :b}
                                 :initial :on
                                 :states {:on {:on {:switch :off}}
                                          :off {}}}
                             :b {:entry :entry-b
                                 :on {:switch :a}}}}))

(deftest interpreter-test
  (testing "Atom based interpreter"
    (let [inst (interpreter the-machine)]
      (println (start inst))
      (println (send-event inst :switch))
      (println (send-event inst :switch)))))
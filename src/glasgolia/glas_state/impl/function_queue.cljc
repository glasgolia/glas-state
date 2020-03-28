(ns glasgolia.glas-state.impl.function-queue
  (:require [#?(:cljs cljs.core.async
                :clj  clojure.core.async) :as ca]))



(defn create-function-queue []
  "Creates a queue that executes functions in order, async.
   Returns a function that can be called with a function to add to the queue,
   or with nil to stop the queue.
   When stopping the queue, a channel is returned that will receive a value
   when all functions left in the queue are done."
  (let [q (atom [])
        queue-chan (ca/chan 10)
        done-chan (ca/chan 1)]
    (ca/go-loop []
      (let [next (ca/<! queue-chan)]
        (if next
          (let [head (ffirst (swap-vals! q pop))]
            (head)
            (recur))
          (do
            (doseq [f @q]
              (f))
            (ca/>! done-chan true)
            (ca/close! done-chan)))))
    (fn [function]
      (if function
        (do
          (swap! q conj function)
          (ca/go (ca/>! queue-chan :function-added))
          nil)
        (do (ca/close! queue-chan)
            done-chan)))))
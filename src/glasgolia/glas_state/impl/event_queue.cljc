(ns glasgolia.glas-state.impl.event-queue
  (:require [clojure.core.async :as ca]))



(defn  create-queue []
  (let [s (ca/chan 1)
        first (ca/chan 1)]
    (ca/>!! s first)
    {:q (atom first)
     :s s}))

(defn service-put [{:keys [queue config state] :as _service} fun]
  (let [s (:s queue)
        prev-chan (ca/<!! s)
        next-chan (ca/chan 1)]
    (ca/go
      (if (ca/<! prev-chan)
        (do
          (if (or (:deferEvents config) @state)
            (fun)
            (ex-message "start is not called on service before event"))
          (ca/>! next-chan true))
        (ca/close! next-chan))
      )
    (ca/>!! s next-chan)))


(defn  start-queue [{:keys [queue] :as _service}]
  (ca/>!! @(:q queue) true))


(defn stop-queue [{:keys [queue] :as _service}]
  (let [s (:s queue)
         prev-chan (ca/<!! s)
         next-chan (ca/chan 1)]
    (ca/close! prev-chan)
    (reset! (:q queue) next-chan)
    (ca/>!! s next-chan )))


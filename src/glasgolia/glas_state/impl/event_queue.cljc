(ns glasgolia.glas-state.impl.event-queue
  (:require [glasgolia.glas-state.impl.function-queue :as fc]))

(def global-queue (fc/create-function-queue))

(defn  create-queue []
  {:started? (atom false)})

(defn service-put [{:keys [queue config state] :as service} fun]
  (let [service-function (fn[]
                           (try
                             (if (or (:deferEvents config) @(:started? queue))
                              (fun)
                              (throw (ex-info "Dispatch event before start is called" {:service service})))
                             (catch #?(:clj Exception
                                       :cljs :default ) e
                               (println e))))]
    (global-queue service-function)))


(defn  start-queue [{:keys [queue] :as _service}]
  (reset! (:started? queue) true))


(defn stop-queue [{:keys [queue] :as _service}]
  (reset! (:started? queue) false))


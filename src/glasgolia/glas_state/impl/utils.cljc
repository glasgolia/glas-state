(ns glasgolia.glas-state.impl.utils)



(defn  todo [msg]
  (ex-message (str "TODO" msg)))




(defn service-log-warn [{:keys [config] :as _service} msg]
  ((:logger config) (str "[WARN] " msg)))

(defn service-log-exception [{:keys [config] :as _service} exception]
  ((:logger config) (prn-str "[ERR]  " exception)))



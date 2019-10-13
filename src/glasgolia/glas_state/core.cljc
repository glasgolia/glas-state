(ns glasgolia.glas-state.core
  (:require [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.service :as i]))

;
;     glasgolia.glas-state public API
;



; from stateless namespace
(def machine-options sl/machine-options)
(def assign sl/assign)
(def send-event sl/send-event)
(def value-to-ids sl/value-to-ids)
(def leaf-value-to-ids sl/leaf-value-to-ids)


; from interpreter namespace
(def service-logger i/service-logger)
(def create-service i/create-service)
(def start i/start)
(def stop i/stop)
(def reset i/reset)
(def dispatch i/dispatch)
#_(def transition-wait i/dispatch-and-wait)
(def state-value i/state-value)


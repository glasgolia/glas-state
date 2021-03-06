(ns glasgolia.glas-state.core
  (:require [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.service :as i]))

;
;     glasgolia.glas-state public API
;



; from stateless namespace
(def assign sl/assign)
(def send-event sl/send-event)
(def send-parent-event sl/send-parent-event)
(def value-to-ids sl/value-to-ids)
(def leaf-value-to-ids sl/leaf-value-to-ids)


; from service namespace

(def service-logger i/service-logger)
(def create-service-logger i/create-service-logger)
(def create-service i/create-service)
(def start i/start)
(def stop i/stop)
(def reset i/reset)
(def dispatch i/dispatch)
(def add-value-reaction i/add-value-reaction)
(def add-context-reaction i/add-context-reaction)


; Access state...
(def service-state i/service-state)
(def service-value i/service-value)
(def service-context i/service-context)
;(def swap-context  i/swap-context)

(ns glasgolia.glas-state.core
  (:require [glasgolia.glas-state.stateless :as sl]
            [glasgolia.glas-state.interpreter :as i]))

;
;     glasgolia.glas-state public API
;



; from stateless namespace
(def machine sl/machine)
(def state-def-machine sl/root-node-machine)
(def assign sl/assign)
(def send-event sl/send-event)
(def value-to-ids sl/value-to-ids)
(def leaf-value-to-ids sl/leaf-value-to-ids)


; from interpreter namespace
(def interpreter-logger i/interpreter-logger)
(def atom-store i/atom-store)
(def interpreter i/interpreter)
(def start i/start)
(def stop i/stop)
(def reset i/reset)
(def add-change-listener i/add-change-listener)
(def transition i/transition)
(def transition-wait i/transition-wait)
(def state-value i/state-value)


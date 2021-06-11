(ns com.vadelabs.machine.core
  (:require
   [com.vadelabs.stack.interface :as vm.stack]
   [com.vadelabs.code.interface :as vm.code]))

(defprotocol IMachine
  (operand-push [this value])
  (operand-pop  [this])
  (data [this idx])
  (execute [this])
  (next-code [this]))

(defrecord Machine [code instruction-table instruction-pointer operand-stack]
  (operand-push [_ value]
    (vm.stack/push operand-stack value))
  (operand-pop [_]
    (vm.stack/pop operand-stack))
  (data [_ idx]
    (vm.code/data code idx))
  (execute [_])
  (next-code [_]))

(defn make-machine
  [code instruction-table]
  (map->Machine {:code code
                 :instruction-table instruction-table
                 :instruction-pointer 0
                 :operand-stack (stack/make-stack)}))

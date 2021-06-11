(ns com.vadelabs.machine.core
  (:require
   [com.vadelabs.stack.interface :as vm.stack]
   [com.vadelabs.code.interface :as vm.code]
   [com.vadelabs.instruction.interface :as vm.instruction]))

(defprotocol IMachine
  (operand-push [this value])
  (operand-pop  [this])
  (operand-peek [this])
  (operand-stack [this])
  (call-stack [this])
  (ip [this])
  (code [this])
  (data [this idx])
  (execute [this])
  (next-code [this]))

(defrecord Machine [code instruction-table ip constants call-stack operand-stack]
  IMachine
  (operand-push [_ value]
    (vm.stack/push operand-stack value))

  (operand-pop [_]
    (vm.stack/pop operand-stack))

  (operand-peek [_]
    (vm.stack/peek operand-stack))

  (data [_ idx]
    (vm.code/code-data code idx))

  (code [this]
    (get-in this [:code]))

  (ip [this]
    (get-in this [:ip]))

  (call-stack [this]
    (get-in this [:call-stack]))

  (operand-stack [this]
    (get-in this [:operand-stack]))

  (next-code [this]
    (let [code (-> this
                   (get-in [:code])
                   (vm.code/code ip))]
      (update-in this [:ip] + 1)
      code))

  (execute [this]))





;; (while (= ip (count (vm.code/code code)))
;;       (let [op-code (next-code this)
;;             arity (next-code this)
;;             instr (vm.instruction/by-op-code instruction-table op-code)
;;             args (into [] (repeatedly arity (next-code this)))
;;             func (vm.instruction/function instr)]
;;         (func this args)))


(defn make-machine
  [code constants instruction-table]
  (let [call-stack (vm.stack/make-stack)]
    (map->Machine {:code code
                   :instruction-table instruction-table
                   :ip 0
                   :constants constants
                   :call-stack call-stack
                   :operand-stack (vm.stack/make-stack)})))

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
  (inc-ip [this])
  (code [this])
  (data [this idx])
  (execute [this])
  (next-code [this]))

(defrecord Machine [code instruction-table ip constants call-stack operand-stack]
  IMachine
  (operand-push [this value]
    (assoc-in this [:operand-stack]
              (vm.stack/push (get-in this [:operand-stack]) value)))

  (operand-pop [this]
    (as-> this $
      (assoc-in $ [:operand-stack] (vm.stack/pop operand-stack))))

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
    (-> this
        (get-in [:code])
        (vm.code/code (get-in this [:ip]))))

  (inc-ip [this]
    (update-in this [:ip] + 1))

  (execute [this]
    (loop [machine this
           iter-cnt 0]
      (if (= iter-cnt (count (vm.code/code code)))
        machine
        (let [op-code (-> machine (next-code))
              arity  (-> machine
                         (inc-ip)
                         (next-code))
              instr (vm.instruction/by-op-code instruction-table op-code)
              [m args] (loop [mach (-> machine (inc-ip) (inc-ip))
                              result []
                              cnt 0]
                         (if (= cnt arity)
                           [mach result]
                           (recur (inc-ip mach) (conj result (next-code mach)) (+ cnt 1))))
              func  (vm.instruction/function instr)
              final-machine (func m args)]
          (recur final-machine (+ iter-cnt arity 2)))))))



  ;; (execute [this]
  ;;   (let [op-code (next-code this)
  ;;         arity (next-code this)]
  ;;     [op-code arity (:ip this)])))





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

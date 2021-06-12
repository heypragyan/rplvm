(ns com.vadelabs.machine.core
  (:require
   [com.vadelabs.stack.interface :as vm.stack]
   [com.vadelabs.code.interface :as vm.code]
   [com.vadelabs.instruction.interface :as vm.instruction]
   [com.vadelabs.machine.frame :as frame]))

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
  (next-code [this])
  (get-local [this n])
  (set-local [this k v])
  (jump [this label])
  (call [this label])
  (ret [this])
  (process-code [this]))

(defrecord Machine [code process-code instruction-table ip constants call-stack operand-stack]
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
    (let [instr-ptr (get-in this [:ip])
          code (-> this
                   (get-in [:code])
                   (vm.code/code instr-ptr))
          new-ip (+ instr-ptr 1)]
      (assoc this :process-code code :ip new-ip)))

  (get-local [this n]
    (-> this
        (get :call-stack)
        (vm.stack/peek)
        (frame/get-local n)))

  (set-local [this k v]
    (let [cs (get this :call-stack)
          fr (vm.stack/peek cs)
          fs (vm.stack/pop cs)
          final-frame (frame/set-local fr k v)]
      (assoc-in this [:call-stack] (vm.stack/push fs final-frame))))

  (jump [this label]
    (assoc-in this [:ip] (-> this
                             (get :code)
                             (vm.code/code-label-ip label))))

  (call [this label]
    (let [ip (get this :ip)
          machine (assoc this :call-stack
                         (vm.stack/push (get this :call-stack)
                                        (frame/make-frame ip)))]
      (jump machine label)))

  (ret [this]
    (let [frame (vm.stack/peek (get this :call-stack))
          machine (assoc this :call-stack (vm.stack/pop (get this :call-stack)))]
      (assoc machine :ip (frame/return-address frame))))

  (execute [this]
    (loop [machine this]
      (if (= (get machine :ip) (count (vm.code/code code)))
        machine
        (let [op-code (-> machine (next-code) (get :process-code))
              arity (-> machine (next-code) (next-code) (get :process-code))
              instr (vm.instruction/by-op-code instruction-table op-code)
              [machine args] (loop [machine (-> machine (next-code) (next-code))
                                    result []]
                               (if (= (count result) arity)
                                 [machine result]
                                 (recur (-> machine (next-code))
                                        (conj result (-> machine (next-code) (get :process-code))))))
              func (vm.instruction/function instr)
              final-machine (func machine args)]
          (recur final-machine))))))

(defn make-machine
  [code constants instruction-table]
  (let [stack-frame (frame/make-frame (count (vm.code/code code)))
        call-stack  (-> (vm.stack/make-stack)
                        (vm.stack/push stack-frame))]
    (map->Machine {:code code
                   :instruction-table instruction-table
                   :ip 0
                   :constants constants
                   :call-stack call-stack
                   :operand-stack (vm.stack/make-stack)})))

(ns com.vadelabs.rpl.interface
  (:require
   [com.vadelabs.machine.interface :as vm.machine]
   [com.vadelabs.instruction.interface :as vm.instruction]
   [com.vadelabs.code.interface :as vm.code]))

(defn push
  [machine args]
  (let [arg (vm.machine/data machine args)]
    (vm.machine/operand-push machine arg)))

(defn add
  [machine _]
  (let [item-1 (vm.machine/operand-pop machine)
        item-2 (vm.machine/operand-pop machine)]
    (vm.machine/operand-push (+ item-1 item-2))))

(defn instruction-table
  []
  (let [it (vm.instruction/make-instruction)]
    (-> it
        (vm.instruction/insert {:op-code 0 :name "push" :arity 1 :func push})
        (vm.instruction/insert {:op-code 1 :name "add" :arity 0 :func add}))))

(defn build-program
  [it]
  (let [builder (vm.code/make-builder it)]
    (-> builder
        (vm.code/push "push" [2])
        (vm.code/push "push" [3])
        (vm.code/push "add" []))))

(defn addition-example
  []
  (let [it (instruction-table)
        code (build-program it)
        machine (vm.machine/make-machine code it)
        machine (vm.machine/execute machine)
        result (vm.machine/operand-pop machine)]
    result))

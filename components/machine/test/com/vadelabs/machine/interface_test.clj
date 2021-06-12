(ns com.vadelabs.machine.interface-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [com.vadelabs.code.interface :as vm.code]
   [com.vadelabs.instruction.interface :as vm.instruction]
   [com.vadelabs.machine.interface :as vm.machine]
   [com.vadelabs.stack.interface :as vm.stack]))

(defn push [machine args]
  (let [arg (-> machine
                (vm.machine/code)
                (vm.code/code-data (get args 0)))]
    (vm.machine/operand-push machine arg)))

(defn add
  [machine _]
  (let [rhs (-> machine
                (vm.machine/operand-peek))
        lhs (-> machine
                (vm.machine/operand-pop)
                (vm.machine/operand-peek))
        machine (-> machine (vm.machine/operand-pop) (vm.machine/operand-pop))]
    (-> machine
        (vm.machine/operand-push (+ lhs rhs)))))

(defn instruction-table
  []
  (-> (vm.instruction/make-instruction-table)
      (vm.instruction/insert (vm.instruction/make-instruction 1 "push" 1 push))
      (vm.instruction/insert (vm.instruction/make-instruction 2 "add" 0 add))))

#_(def it (instruction-table))
#_(def builder (-> (vm.code/make-builder it)
                   (vm.code/push "push" [3])
                   (vm.code/push "push" [2])
                   (vm.code/push "add"  [])))
#_(def constants {})
#_(def machine (vm.machine/make-machine (vm.code/make-code builder) constants it))
#_(tap> machine)
#_(vm.machine/execute machine)

(deftest test-machine-implmentation
  (testing "initialize new stack machine"
    (let [it (instruction-table)
          builder (vm.code/make-builder it)
          constants {}
          machine (vm.machine/make-machine (vm.code/make-code builder) constants it)]
      (is (= 0 (vm.machine/ip machine)))
      (is (-> machine (vm.machine/call-stack) (vm.stack/empty?)))
      (is (-> machine (vm.machine/operand-stack) (vm.stack/empty?)))))
  (testing "executing the stack machine"
    (let [it (instruction-table)
          builder (->  (vm.code/make-builder it)
                       (vm.code/push "push" [2])
                       (vm.code/push "push" [3])
                       (vm.code/push "add" []))
          constants {}
          machine (vm.machine/make-machine (vm.code/make-code builder) constants it)
          final-machine (vm.machine/execute machine)]
      (is (= 5 (vm.machine/operand-peek final-machine))))))

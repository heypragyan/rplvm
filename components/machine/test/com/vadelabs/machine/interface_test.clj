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

(defn call
  [machine args]
  (let [label (-> machine
                  (vm.machine/data (get args 0)))]
    (vm.machine/call machine label)))

(defn ret
  [machine _]
  (vm.machine/ret machine))

(defn jump
  [machine args]
  (let [label (-> machine
                  (vm.machine/data (get args 0)))]
    (vm.machine/jump machine label)))

(defn jump-if
  [machine args]
  (let [condition (-> machine
                      (vm.machine/operand-peek))
        fmachine (-> machine
                     (vm.machine/operand-pop))]
    (if-not (= condition 0)
      (let [label (-> fmachine
                      (vm.machine/data (get args 0)))]
        (vm.machine/jump fmachine label))
      fmachine)))

(defn instruction-table
  []
  (-> (vm.instruction/make-instruction-table)
      (vm.instruction/insert (vm.instruction/make-instruction 1 "push" 1 push))
      (vm.instruction/insert (vm.instruction/make-instruction 2 "add" 0 add))
      (vm.instruction/insert (vm.instruction/make-instruction 3 "call" 1 call))
      (vm.instruction/insert (vm.instruction/make-instruction 4 "ret" 0 ret))
      (vm.instruction/insert (vm.instruction/make-instruction 5 "jump-if" 1 jump-if))
      (vm.instruction/insert (vm.instruction/make-instruction 6 "jump" 1 jump))))

#_(def it (instruction-table))
#_(def builder (-> (vm.code/make-builder it)
                   (vm.code/push "push" [3])
                   (vm.code/push "push" [2])
                   (vm.code/push "add"  [])))
#_(def constants {})
#_(def machine (vm.machine/make-machine (vm.code/make-code builder) constants it))
#_(tap> machine)
#_(vm.machine/execute machine)

(deftest test-simple-machine-implmentation
  (testing "initialize new stack machine"
    (let [it (instruction-table)
          builder (vm.code/make-builder it)
          constants {}
          machine (vm.machine/make-machine (vm.code/make-code builder) constants it)]
      (is (= 0 (vm.machine/ip machine)))
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
      (is (= 5 (vm.machine/operand-peek final-machine)))))
  (testing "get-local in call-stack"
    (let [it (instruction-table)
          builder (-> (vm.code/make-builder it))
          constants {}
          machine (vm.machine/make-machine (vm.code/make-code builder) constants it)
          final-machine (vm.machine/set-local machine "example" 13)]
      (is (nil? (vm.machine/get-local machine "example")))
      (is (= 13 (vm.machine/get-local final-machine "example"))))))

#_(def it (instruction-table))
#_(def builder (-> (vm.code/make-builder it)
                   (vm.code/push "push" [3])
                   (vm.code/push "push" [4])
                   (vm.code/push "call" ["add-func"])
                   (vm.code/push "ret" [])
                   (vm.code/add-label "add-func")
                   (vm.code/push "add" [])
                   (vm.code/push  "ret" [])))
#_(def constants {})
#_(def machine (vm.machine/make-machine (vm.code/make-code builder) constants it))
#_(-> machine (vm.machine/next-code))
#_(tap> machine)
#_(vm.machine/execute machine)

#_(def it (instruction-table))
#_(defn cond-prog [condition]
    (let [builder (-> (vm.code/make-builder it)
                      (vm.code/push "push" [condition])
                      (vm.code/push "jump-if" ["if-true"])
                      (vm.code/push "push" ["it was false"])
                      (vm.code/push "jump" ["end"])
                      (vm.code/add-label "if-true")
                      (vm.code/push "push" ["it was true"])
                      (vm.code/add-label "end"))
          constants {}
          machine (vm.machine/make-machine (vm.code/make-code builder) constants it)]
      (vm.machine/execute machine)))
#_(cond-prog 0)

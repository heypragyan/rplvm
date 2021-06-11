(ns
 (:require
  [clojure.test :refer [deftest testing is are]]
  [com.vadelabs.instruction.interface :as instr]))

(defn noop
  [machine & args])

(deftest test-instruction
  (testing "initialize new instruction"
    (let [operand (instr/make-instruction 12 "noop" 3 noop)]
      (is (= 12 (:op-code operand)))
      (is (= "noop" (:name operand)))
      (is (= 3 (:arity operand))))))

(deftest test-instruction-table
  (testing "initialize new instruction table"
    (let [instr-table (instr/make-instruction-table)]
      (is (instr/empty? instr-table))))
  (testing "insert instruction in the table"
    (let [instr-table-1 (instr/make-instruction-table)
          instr-item (instr/make-instruction 0  "noop"  0  noop)
          instr-table-2 (instr/insert instr-table-1 instr-item)]
      (is (instr/empty? instr-table-1))
      (is (not (instr/empty? instr-table-2)))))
  (testing "retrieve instruction by op-code"
    (let [instr-table-1 (instr/make-instruction-table)
          instr-item (instr/make-instruction 0  "noop"  0  noop)
          instr-table-2 (instr/insert instr-table-1 instr-item)]
      (is (instr/empty? instr-table-1))
      (is (not (instr/empty? instr-table-2)))
      (is (= instr-item (instr/by-op-code instr-table-2 0)))))
  (testing "retrieve instruction by name"
    (let [instr-table-1 (instr/make-instruction-table)
          instr-item (instr/make-instruction 0 "noop" 0 noop)
          instr-table-2 (instr/insert instr-table-1 instr-item)]
      (is (instr/empty? instr-table-1))
      (is (not (instr/empty? instr-table-2)))
      (is (= instr-item (instr/by-name instr-table-2 "noop"))))))

#_(-> (instr/make-instruction-table)
      (instr/insert (instr/make-instruction 0 "noop" 0 noop))
      (instr/symbols))


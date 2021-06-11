(ns com.vadelabs.code.interface-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [com.vadelabs.code.interface :as vm.code]
   [com.vadelabs.instruction.interface :as vm.instruction]))

(defn noop
  [machine & args])

(defn example-it
  []
  (-> (vm.instruction/make-instruction-table)
      (vm.instruction/insert (vm.instruction/make-instruction 0 "noop" 0 noop))
      (vm.instruction/insert (vm.instruction/make-instruction 1 "push" 1 noop))
      (vm.instruction/insert (vm.instruction/make-instruction 2 "pop"  0 noop))))

#_(-> (example-it)
      (vm.code/make-builder)
      (vm.code/push "push" [123])
      (vm.code/push "push" [123])
      (vm.code/push "push" [123]))

(deftest test-builder
  (testing "initialize empty builder"
    (let [it (example-it)
          builder (vm.code/make-builder it)]
      (is (empty? (vm.code/instructions builder)))))
  (testing "push instruction to builder"
    (let [builder (-> (example-it)
                      (vm.code/make-builder)
                      (vm.code/push "noop" []))]
      (is (seq (vm.code/instructions builder)))))
  (testing "label"
    (let [builder (-> (example-it)
                      (vm.code/make-builder)
                      (vm.code/push "noop" [])
                      (vm.code/add-label "wow"))]
      (is (= 2 (-> builder
                   (vm.code/labels)
                   (get "wow"))))))
  (testing "data is deduped"
    (let [builder (-> (example-it)
                      (vm.code/make-builder)
                      (vm.code/push "push" [123])
                      (vm.code/push "push" [123])
                      (vm.code/push "push" [123]))]
      (is (= 1 (count (vm.code/builder-data builder)))))))

#_(-> (example-it)
      (vm.code/make-builder)
      (vm.code/push "push" [13])
      (vm.code/push "push" [14])
      (vm.code/make-code)
      (vm.code/code-symbols))

(deftest test-make-code
  (testing "build code from builder"
    (let [code (-> (example-it)
                   (vm.code/make-builder)
                   (vm.code/push "push" [13])
                   (vm.code/push "push" [14])
                   (vm.code/make-code))]
      (is (= 3 (count (vm.code/code-symbols code))))
      (is (= [[0 "noop"] [1 "push"] [2 "pop"]] (vm.code/code-symbols code)))
      (is (= [1 1 0 1 1 1] (vm.code/code code)))
      (is (= [13 14] (vm.code/code-data code)))
      (is (= 1 (count (vm.code/code-labels code))))
      (is (= [[0 "main"]] (vm.code/code-labels code)))))
  (testing "retrieve label instruction pointer"
    (let [code (-> (example-it)
                   (vm.code/make-builder)
                   (vm.code/make-code))]
      (is (= 0 (vm.code/code-label-ip code "main"))))))

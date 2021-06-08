(ns com.vadelabs.stack.interface-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [com.vadelabs.stack.interface :as stack]))

(deftest test-stack-operations
  (testing "initialize new stack"
    (let [s (stack/new-stack)]
      (is (stack/empty? s))))
  (testing "push value to a stack"
    (let [s-1 (stack/new-stack)
          s-2 (stack/push s-1 12)
          s-3 (-> (stack/new-stack)
                  (stack/push 1)
                  (stack/push 2)
                  (stack/push 3))]
      (is (not (stack/empty? s-2)))
      (is (= '(12) (stack/get s-2)))
      (is (= '(3 2 1) (-> s-3 stack/get)))))
  (testing "pop value from the stack"
    (let [s-1 (stack/new-stack)
          s-2 (stack/push s-1 13)
          s-3 (stack/pop s-2)
          s-4 (-> (stack/new-stack)
                  (stack/push 1)
                  (stack/push 2)
                  (stack/push 3)
                  (stack/pop))]
      (is (= '(13) (stack/get s-2)))
      (is (stack/empty? s-3))
      (is (= '(2 1) (-> s-4 stack/get)))))
  (testing "push values - variadic args"
    (let [s-1 (-> (stack/new-stack)
                  (stack/push 1 2 3 4))]
      (is (= '(4 3 2 1) (-> s-1 stack/get))))))

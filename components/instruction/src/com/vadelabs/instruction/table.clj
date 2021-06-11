(ns com.vadelabs.instruction.table
  (:refer-clojure :exclude [empty?])
  (:require
   [com.vadelabs.instruction.core :as instruction]))

(defprotocol ITable
  (by-op-code [this op-code])
  (by-name [this n])
  (insert [this instr])
  (empty? [this])
  (symbols [this]))

(defrecord Table [table]
  ITable
  (by-op-code [this op-code]
    (get-in this [:table op-code]))

  (by-name [this n]
    (first (filter (fn [item]
                     (= n (instruction/get-name item)))
                   (vals (get-in this [:table])))))

  (insert [this instr]
    (assoc-in this [:table (:op-code instr)] instr))

  (empty? [this]
    (clojure.core/empty? (:table this)))

  (symbols [this]
    (let [result (map (fn [k]
                        (let [instr (get-in this [:table k])]
                          [(instruction/get-op-code instr) (instruction/get-name instr)]))
                      (keys table))]
      (sort-by first result))))

(defn make-instruction-table
  []
  (map->Table {:table {}}))

#_(-> (new-table)
      (insert (instr/new-instruction {:op-code 0 :name "noop" :arity 0 :func (fn [])}))
      (by-name "noop"))

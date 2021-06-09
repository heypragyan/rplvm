(ns com.vadelabs.instruction.table
  (:refer-clojure :exclude [empty?])
  (:require
   [com.vadelabs.instruction.core :as instr]))

(defprotocol ITable
  (by-op-code [this op-code])
  (by-name [this n])
  (insert [this instr])
  (empty? [this]))

(defrecord Table [table]
  ITable
  (by-op-code [this op-code]
    (get-in this [:table op-code]))

  (by-name [this n]
    (first (filter (fn [item]
                     (= n (instr/get-name item)))
                   (vals (get-in this [:table])))))

  (insert [this instr]
    (assoc-in this [:table (:op-code instr)] instr))

  (empty? [this]
    (clojure.core/empty? (:table this))))

(defn new-table
  []
  (map->Table {:table {}}))

#_(-> (new-table)
      (insert (instr/new-instruction {:op-code 0 :name "noop" :arity 0 :func (fn [])}))
      (by-name "noop"))

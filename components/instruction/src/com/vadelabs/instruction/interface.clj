(ns com.vadelabs.instruction.interface
  (:refer-clojure :exclude [empty? name])
  (:require
   [com.vadelabs.instruction.core :as instruction]
   [com.vadelabs.instruction.table :as it]))

(def make-instruction instruction/make-instruction)
(def make-instruction-table it/make-instruction-table)

(defn name
  [instr]
  (instruction/get-name instr))

(defn op-code
  [instr]
  (instruction/get-op-code instr))

(defn arity
  [instr]
  (instruction/arity instr))

(defn by-op-code
  [table op-code]
  (it/by-op-code table op-code))

(defn by-name
  [table n]
  (it/by-name table n))

(defn insert
  [table instr]
  (it/insert table instr))

(defn empty?
  [table]
  (it/empty? table))

(defn symbols
  [table]
  (it/symbols table))

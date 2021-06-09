(ns com.vadelabs.instruction.interface
  (:refer-clojure :exclude [empty?])
  (:require
   [com.vadelabs.instruction.core :as instruction]
   [com.vadelabs.instruction.table :as table]))

(defn new-instruction
  [options]
  (instruction/new-instruction options))

(defn new-table
  []
  (table/new-table))

(defn by-op-code
  [table op-code]
  (table/by-op-code table op-code))

(defn by-name
  [table n]
  (table/by-name table n))

(defn insert
  [table instr]
  (table/insert table instr))

(defn empty?
  [table]
  (table/empty? table))

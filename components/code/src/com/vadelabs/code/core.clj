(ns com.vadelabs.code.core
  (:refer-clojure :exclude [empty])
  (:require
   [com.vadelabs.code.builder :as vm.builder]
   [com.vadelabs.instruction.interface :as vm.instruction]))

(defprotocol ICode
  (symbols [this])
  (-code [this])
  (-data [this])
  (labels [this])
  (label-ip [this item]))

(defrecord Code [symbols code data labels]
  ICode
  (symbols [this]
    (get-in this [:symbols]))
  (-code [this]
    (get-in this [:code]))
  (-data [this]
    (get-in this [:data]))
  (labels [this]
    (get-in this [:labels]))
  (label-ip [this item]
    (ffirst (filter
             (fn [label]
               (= (second label) item))
             (get-in this [:labels])))))

(defn data
  ([code]
   (-> code (-data)))
  ([code idx]
   (-> code (-data) (get idx))))

(defn code
  ([code]
   (-> code (-code)))
  ([code idx]
   (-> code (-code) (get idx))))

(defn make-code
  ([]
   (map->Code {:symbols []
               :code []
               :data []
               :labels []}))
  ([builder]
   (let [symbols (into [] (-> builder
                              (vm.builder/instruction-table)
                              (vm.instruction/symbols)))
         code (-> builder
                  (vm.builder/instructions))
         data (-> builder
                  (vm.builder/data))
         label-map (-> builder (vm.builder/labels))
         labels (map (fn [label] [(get label-map label) label]) (keys label-map))
         labels (into [] (sort-by first labels))]
     (map->Code {:symbols symbols
                 :code code
                 :data data
                 :labels labels}))))

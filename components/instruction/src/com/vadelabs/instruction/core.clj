(ns com.vadelabs.instruction.core)

(defprotocol IInstruction
  (get-op-code [this])
  (get-name [this])
  (arity [this])
  (function [this]))

(defrecord Instruction [op-code name arity func]
  IInstruction
  (get-op-code [this]
    (get-in this [:op-code]))
  (get-name [this]
    (get-in this [:name]))
  (arity [this] (get this :arity))
  (function [this] (get this :function)))

(defn make-instruction
  [op-code name arity func]
  (map->Instruction {:op-code op-code
                     :name name
                     :arity arity
                     :function func}))

#_(-> (new-instruction {:op-code 0
                        :name "noop"
                        :arity 0
                        :func (fn [])})
      (get-name))

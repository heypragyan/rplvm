(ns com.vadelabs.instruction.core)

(defprotocol IInstruction
  (get-op-code [this])
  (get-name [this]))

(defrecord Instruction [op-code name arity func]
  IInstruction
  (get-op-code [this]
    (get-in this [:op-code]))
  (get-name [this]
    (tap> ["TEST"])
    (get-in this [:name])))

(defn new-instruction
  [{:keys [op-code name arity func]}]
  (map->Instruction {:op-code op-code
                     :name name
                     :arity arity
                     :func func}))

#_(-> (new-instruction {:op-code 0
                        :name "noop"
                        :arity 0
                        :func (fn [])})
      (get-name))

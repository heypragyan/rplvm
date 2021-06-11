(ns com.vadelabs.code.builder
  (:require
   [com.vadelabs.instruction.interface :as vm.instruction]
   [com.vadelabs.stack.interface :as vm.stack]))

(defprotocol IBuilder
  (push [this item args])
  (push-data [this args])
  (instruction-table [this])
  (instructions [this])
  (labels [this])
  (add-label [this item])
  (data [this])
  (lenth [this]))

(defrecord Builder [instruction-table instructions labels data]
  IBuilder
  (push-data [this args]
    (let [data-items (apply conj (get-in this [:data]) args)]
      (-> this
          (assoc-in [:data] (into [] (distinct data-items))))))

  (push [this item args]
    (let [it (get-in this [:instruction-table])
          instr (vm.instruction/by-name it item)
          arity (vm.instruction/arity instr)
          _ (when-not instr
              (throw (Exception. (ex-info "Unable to find instruction" {:name item}))))
          _ (when-not (= (count args) arity)
              (throw (Exception. (ex-info "Instruction has different arity than passed arguments"
                                          {:name item
                                           :arity arity
                                           :args (count args)}))))
          builder (-> this (push-data args))
          indices (map (fn [arg]
                         (.indexOf (-> builder (get-in [:data])) arg))
                       args)
          ;; indices (map-indexed (fn [idx _] idx) (-> builder (get-in [:data])))
          instrs (apply conj instructions
                        (vm.instruction/op-code instr)
                        (vm.instruction/arity instr)
                        indices)]
      (assoc-in builder [:instructions] instrs)))

  (instruction-table [this]
    (get-in this [:instruction-table]))
  (instructions [this]
    (get-in this [:instructions]))
  (labels [this]
    (get-in this [:labels]))
  (data [this]
    (get-in this [:data]))
  (add-label [this item]
    (let [idx (count instructions)]
      (update-in this [:labels] assoc item idx))))

#_(def s {:qw {:b 1}})
#_(update-in s [:wq] assoc :h :w)


;; (defn push
;;   ([b x]
;;    (-> b
;;        (-push x)))
;;   ([b x1 x2]
;;    (-> b
;;        (-push x1)
;;        (-push x2)))
;;   ([b x1 x2 x3]
;;    (-> b
;;        (-push x1)
;;        (-push x2)
;;        (-push x3)))
;;   ([b x1 x2 x3 & xs]
;;    (-> b
;;        (-push x1)
;;        (-push x2)
;;        (-push x3)
;;        (as-> $ (reduce -push $ xs)))))


(defn make-builder
  [instruction-table]
  (map->Builder {:instruction-table instruction-table
                 :instructions []
                 :labels {"main" 0}
                 :data []}))

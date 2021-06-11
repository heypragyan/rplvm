(ns com.vadelabs.stack.core
  (:refer-clojure :exclude [empty? pop get set empty peek]))

(defprotocol IStack
  (empty? [this])
  (push [this item])
  (pop [this])
  (peek [this])
  (get [this])
  (set [this stack])
  (clear-stack [this]))

(defrecord Stack [stack]
  IStack

  (empty? [this]
    (clojure.core/empty? (get-in this [:stack])))

  (push [this item]
    (update-in this [:stack] conj item))

  (pop [this]
    (as-> this $
      (update-in $ [:stack] clojure.core/pop)))

  (peek [this]
    (first (get-in this [:stack])))

  (get [this]
    (-> this :stack))

  (set [this stack]
    (assoc this :stack stack))

  (clear-stack [this]
    (assoc this :stack (clojure.core/empty (:stack this)))))

#_(def s '(1 2 3))
#_(conj s 4)

(defn make-stack []
  (map->Stack {:stack '()}))

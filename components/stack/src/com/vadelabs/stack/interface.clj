(ns com.vadelabs.stack.interface
  (:refer-clojure :exclude [empty? pop get set peek])
  (:require
   [com.vadelabs.stack.core :as stack]))

(defn make-stack
  []
  (stack/make-stack))

(defn empty?
  [s]
  (stack/empty? s))

(defn push
  ([s x] (stack/push s x))
  ([s x1 x2]
   (-> s
       (stack/push x1)
       (stack/push x2)))
  ([s x1 x2 x3]
   (-> s
       (stack/push x1)
       (stack/push x2)
       (stack/push x3)))
  ([s x1 x2 x3 & xs]
   (-> s
       (stack/push x1)
       (stack/push x2)
       (stack/push x3)
       (as-> $ (reduce stack/push $ xs)))))

(defn pop
  [s]
  (stack/pop s))

(defn peek
  [s]
  (stack/peek s))

(defn get
  [s]
  (stack/get s))

(defn set
  [s stk]
  (stack/set s stk))

(defn clear-stack
  [s]
  (stack/clear-stack s))

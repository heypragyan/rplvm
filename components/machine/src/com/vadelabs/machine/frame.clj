(ns com.vadelabs.machine.frame)

(defprotocol IFrame
  (return-address [this])
  (get-local [this n])
  (set-local [this k v]))

(defrecord Frame [locals return-address]
  IFrame
  (return-address [this]
    (get-in this [:return-address]))

  (get-local [this n]
    (get-in this [:locals n]))

  (set-local [this k v]
    (assoc-in this [:locals k] v)))

(defn make-frame
  [return-address]
  (map->Frame {:locals {}
               :return-address return-address}))

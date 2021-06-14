(ns com.vadelabs.rpl.interface
  (:refer-clojure :exclude [pop])
  (:require
   [com.vadelabs.machine.interface :as vm.machine]
   [com.vadelabs.instruction.interface :as vm.instruction]
   [com.vadelabs.code.interface :as vm.code]
   [clojure.spec.alpha :as s]
   [clojure.string :as cstr]
   [clojure.edn :as edn]))

(defn push
  [machine args]
  (let [arg (-> machine
                (vm.machine/data (get args 0)))]
    (vm.machine/operand-push machine arg)))

(defn pop
  [machine _]
  (vm.machine/operand-pop machine))

(defn variable
  [machine args]
  (let [value (-> machine
                  (vm.machine/data (get args 1)))
        varname (-> machine
                    (vm.machine/data (get args 0)))]
    (vm.machine/set-local machine varname value)))

(defn invoke
  [machine args]
  (let [arity (-> machine
                  (vm.machine/data (get args 1)))
        func (-> machine
                 (vm.machine/data (get args 0)))
        [machine items] (loop [machine machine
                               result []]
                          (if (= (count result) arity)
                            [machine result]
                            (recur (-> machine (vm.machine/operand-pop)) (conj result (-> machine (vm.machine/operand-peek))))))]
    (-> machine
        (vm.machine/operand-push (apply func items)))))

(defn assign-var
  [machine args]
  (let [key (-> machine
                (vm.machine/data (get args 0)))
        value (-> machine
                  (vm.machine/operand-peek))]
    (vm.machine/set-local machine key value)))

(defn get-var
  [machine args]
  (let [key (-> machine
                (vm.machine/data (get args 0)))
        value (vm.machine/get-local machine key)]
    (vm.machine/operand-push machine value)))

(defn call
  [machine args]
  (let [label (-> machine
                  (vm.machine/data (get args 0)))]
    (vm.machine/call machine label)))

(defn ret
  [machine _]
  (vm.machine/ret machine))

(defn jump
  [machine args]
  (let [label (-> machine
                  (vm.machine/data (get args 0)))]
    (vm.machine/jump machine label)))

(defn jump-if
  [machine args]
  (let [condition (-> machine
                      (vm.machine/operand-peek))
        machine (-> machine
                    (vm.machine/operand-pop))]
    (if condition
      (let [label (-> machine
                      (vm.machine/data (get args 0)))]
        (vm.machine/jump machine label))
      machine)))

(defn instruction-table
  []
  (-> (vm.instruction/make-instruction-table)
      (vm.instruction/insert (vm.instruction/make-instruction 0 "push" 1 push))
      (vm.instruction/insert (vm.instruction/make-instruction 1 "<pop>" 0 pop))
      (vm.instruction/insert (vm.instruction/make-instruction 2 "call" 1 call))
      (vm.instruction/insert (vm.instruction/make-instruction 3 "ret" 0 ret))
      (vm.instruction/insert (vm.instruction/make-instruction 4 "if>" 1 jump-if))
      (vm.instruction/insert (vm.instruction/make-instruction 5 "jump" 1 jump))
      (vm.instruction/insert (vm.instruction/make-instruction 6 "invoke>" 2 invoke))
      (vm.instruction/insert (vm.instruction/make-instruction 7 "variable" 2 variable))
      (vm.instruction/insert (vm.instruction/make-instruction 8 "assign-var" 1 assign-var))
      (vm.instruction/insert (vm.instruction/make-instruction 9 "get-var" 1 get-var))))

(s/def ::args (s/cat
               :sym symbol?
               :doc (s/? string?)
               :arglist vector?
               :options (s/? map?)
               :body (s/* any?)))

(defn sym->op
  [item]
  (cond
    (-> item (str) (cstr/starts-with? "!")) ["push" [item]]
    :else ["push" [item]]))

(defn remove-last [str]
  (.substring (java.lang.String. str) 0 (- (count str) 1)))

(defn get-op
  [current others]
  (let [others (if (empty? others) [] others)]
    (cond
      (= current "if>") ["if>" [] others]
      (= current "<pop>") ["<pop>" [] others]
      (and (cstr/starts-with? current "!") (cstr/ends-with? current "+")) ["assign-var" [(remove-last current)] others]
      (cstr/starts-with? current "!") ["get-var" [current] others]
      (= "invoke>" current) ["invoke>" [(-> others first (symbol) (resolve)) (-> others rest first (edn/read-string))] (-> others rest rest)]
      :else ["push" [current] others])))

(defn get-cond-block
  [others]
  (let [[truthy others] (loop [current (first others)
                               others (rest others)
                               result []]
                          (if (= "else>" current)
                            [result others]
                            (recur (first others) (rest others) (conj result current))))
        [falsy others] (loop [current (first others)
                              others (rest others)
                              result []]
                         (if (or (nil? current) (= "endif>" current))
                           [result others]
                           (recur (first others) (rest others) (conj result current))))]
    [truthy falsy others]))

(declare eval-builder)

(defn get-builder
  [builder op current others]
  (cond
    (= op "if>")
    (let [[truthy falsy others] (get-cond-block others)
          builder (-> builder
                      (vm.code/push "if>" ["if-true"]))
          builder (-> builder
                      (eval-builder falsy))
          builder (-> builder
                      (vm.code/push "jump" ["end"])
                      (vm.code/add-label "if-true"))
          builder (-> builder
                      (eval-builder truthy)
                      (vm.code/add-label "end"))]
      [builder others])

    :else [(-> builder (vm.code/push op current)) others]))

(defn eval-builder
  [builder instructions]
  (loop [builder builder
         current (first instructions)
         others (rest instructions)]
    (if (nil? current)
      builder
      (let [[op current others] (get-op current others)
            [builder others] (get-builder builder op current others)]
        (recur builder (first others) (rest others))))))

(defn eval-str
  [str-val]
  (let [instructions str-val
        it (instruction-table)
        builder (-> it
                    (vm.code/make-builder))
        builder (eval-builder builder instructions)
        machine (vm.machine/make-machine (vm.code/make-code builder) {} it)
        machine (vm.machine/execute machine)]
    ;;(tap> ["machine" machine])
    (vm.machine/operand-peek machine)))

#_(def strv "1 2 invoke> + 2 !v1+ 4 4 <pop> 2 invoke> * 2 !v2+ invoke> = 2 if> 'hello' else> 'false!!' invoke> println 1 <pop> !v1 !v2 invoke> * 2 !v4")
#_(eval-str strv)

(defn stringify-instruction
  [item arglist]
  (cond
    (list? item)
    (apply conj [(-> item (first) (str))] (flatten (map (fn [i]
                                                          (cond
                                                            (list? i) (stringify-instruction i arglist)
                                                            :else
                                                            (str i))) (rest item))))
    (and (cstr/starts-with? (str item) "!") (cstr/ends-with? (str item) "+"))
    [(str item)]
    (and (cstr/starts-with? (str item) "!") (some #(= item %) arglist))
    [item]
    (cstr/starts-with? (str item) "!")
    [(str item)]
    (= (str item) "<pop>")
    [(str item)]
    :else
    [item]))

#_(apply conj [] [2])

(defn stringify-body
  [body arglist]
  (loop [current (first body)
         others (rest body)
         result []]
    (if (nil? current)
      result
      (let [item (stringify-instruction current arglist)]
        (recur (first others) (rest others) (apply conj result item))))))

(defn defstackfn*
  [_ args]
  (when-not (s/valid? ::args args)
    (throw (Exception. (ex-info "Invalid arguments. " (-> (s/explain-data ::args args)
                                                          ::s/problems
                                                          first
                                                          :path)
                                "is invalid."))))
  (let [{:keys [sym doc arglist options body]} (s/conform ::args args)
        instrs (stringify-body body arglist)]
    `(defn ~sym ~arglist
       (eval-str ~instrs))))

(defmacro defstackfn
  [& args]
  (defstackfn* &env args))

#_(defstackfn f
    [!a !b !c]
    !a
    !b
    (invoke> + 2))

#_(macroexpand-1 '(defstackfn f [!a !b !c]
                    !a
                    !b
                    (invoke> + 2)))

#_(f 4 5 5)
#_(eval-str (f 4 5 5))

#_(defstackfn f
    [!a !b !c]
    !a
    !b
    (invoke> + 2)
    !v1+
    !c
    !c
    <pop>
    2
    (invoke> * 2)
    !v2+
    (invoke> = 2)
    (if>
     !v1
     !v2
     (invoke> - 2)
     else>
     "false!!"
     (invoke> println 1)
     <pop>
     !v1
     !v2
     (invoke> * 2)))
#_(f 1 2 4)

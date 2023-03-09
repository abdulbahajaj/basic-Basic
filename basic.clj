(ns basic)

(defn create-ctx []
  {:tokens []
   :var-count 0
   :env {}
   :uvm-code []
   :loops {}})

(def token-patterns
  [[::skip #"^ "]
   [::num #"^[0-9]+"]
   [::== #"^=="]
   [::!= #"^!="]
   [::> #"^>"]
   [::< #"^<"]
   [::>= #"^>="]
   [::<= #"^<="]
   [::+ #"^\+"]
   [::left-paren #"^\("]
   [::right-paren #"^\)"]
   [::- #"^\-"]
   [::* #"^\*"]
   [::div #"^/"]
   [::! #"^\!"]
   [::new-line #"^\n"]
   [::sym #"^[a-zA-Z]+"]])

(defn get-token [s [token-type pattern]]
  (when-let [match (re-find pattern s)]
    [(subs s (count match)) {:type token-type :str match}]))

(defn token-seq 
  ([input]
   (if-let [[rem token] (some (partial get-token input) token-patterns)]
     (if (empty? rem) [token]
       (lazy-seq (cons token (token-seq rem))))
     (throw (Exception. (str "unrecognized token: " input))))))

(defn tokenize [ctx input]
  (->> input
       token-seq
       (remove #(= ::skip (:type %)))
       (assoc ctx :tokens)))

(defn get-stack-pos [ctx dst]
  (if (keyword? dst)
    (recur ctx (get-in ctx [:env dst :stack-pos]))
    dst))

(defn emit-code [ctx & strs]
  (update ctx :uvm-code conj (apply str (conj strs "\n"))))

(defn emit-push [ctx n]
  (emit-code ctx "push " (str n) ";"))

(defn emit-set-local [ctx dst]
  (emit-code ctx "set_local " (str (get-stack-pos ctx dst))";"))

(defn emit-get-local [ctx src]
  (emit-code ctx "get_local " (str (get-stack-pos ctx src)) ";"))

(defn emit-label [ctx name]
  (if (nil? name) ctx
      (emit-code ctx "label_" name ":")))

(defn emit-goto [ctx dst-label]
  (emit-code ctx "jmp label_" dst-label ";"))

(def binary-op->uvm-inst
  {::+ "add_u64"
   ::- "sub_u64"
   ::== "eq_u64"
   ::!= "nq_u64"
   ::> "gt_i64"
   ::< "lt_i64"
   ::>= "ge_i64"
   ::<= "le_i64"
   ::* "mul_u64"
   ::div "div_i64"})

(defn emit-binary-op [ctx op dst lhs rhs]
  (-> ctx
      (emit-get-local lhs)
      (emit-get-local rhs)
      (emit-code (binary-op->uvm-inst op) ";")
      (emit-set-local dst)))

(defn emit-call [ctx fn args]
  (emit-code
   (reduce emit-get-local ctx args)
   "call " fn ", " (count args) ";"))

(defn emit-branch-if-not [ctx src dst-label]
  (-> ctx
      (emit-get-local src)
      (emit-code "jz label_" dst-label ";")))

(defn emit-branch-if [ctx src dst-label]
  (-> ctx
      (emit-get-local src)
      (emit-code "jnz label_" dst-label ";")))

(defn emit-inc [ctx dst]
  (-> ctx
      (emit-push 1)
      (emit-get-local dst)
      (emit-code "add_u64;")
      (emit-set-local dst)))

(defn alloc-var [ctx]
  [(update ctx :var-count inc) (:var-count ctx)])

(defn expect-num [[t & rem]]
  (assert (= ::num (:type t))
          (str "expected an integer but got " t))
  [t rem])

(defn expect-sym
  ([tokens expected-val]
   (let [[{actual-val :str :as cur-token} tokens] (expect-sym tokens)]
     (assert (= actual-val expected-val)
             (str "Expected to find " expected-val " but got " cur-token))
     [cur-token tokens]))
  ([[t & rem]]
   (assert (= ::sym (:type t))
           (str "expected a sym but got " t))
   [t rem]))

(defn alloc-num [ctx tokens]
  (let [[ctx pos](alloc-var ctx)
        [{:keys [str]} rem] (expect-num tokens)]
    [pos
     (-> ctx
         (emit-push str)
         (emit-set-local pos))
     rem]))

(defn derive-list [parent l]
  (doall (map #(derive % parent) l)))

(derive-list ::equality [::!= ::==])
(derive-list ::comparison [::> ::< ::>= ::<=])
(derive-list ::term [::+ ::-])
(derive-list ::factor [::div ::*])
(derive-list ::uni [::- ::!])
(derive-list ::binary-op [::equality ::comparison ::term ::factor])

(declare emit-equality)

(defn deref-sym [ctx tokens]
  (let [[{:keys [str] :as cur-token} rem] (expect-sym tokens)
        pos (get-in ctx [:env (keyword str) :stack-pos])]
    (assert pos (str "Symbol " cur-token " is used before it was defined"))
    [pos ctx rem]))

(defn emit-prim [ctx [{:keys [type] :as cur-token} & rem :as tokens]]
  (condp = type
    ::num (alloc-num ctx tokens)
    ::sym (deref-sym ctx tokens)
    ::left-paren
    (let [[pos ctx [{:keys [type] :as cur-token} & rem]] (emit-equality ctx rem)]
      (assert (= type ::right-paren) (str "bracket is not closed: " cur-token))
      [pos ctx rem])))

(defn parse-and-emit-binary-op [ctx cur-type higher-prec-fn tokens]
  (loop [[lhs-pos ctx [{:keys [type]} & rem :as tokens]] (higher-prec-fn ctx tokens)]
    (if (isa? type cur-type)
      (let [[rhs-pos ctx tokens] (higher-prec-fn ctx rem)
            [ctx dst-pos] (alloc-var ctx)]
        (recur [dst-pos
                (emit-binary-op ctx type dst-pos lhs-pos rhs-pos)
                tokens]))
      [lhs-pos ctx tokens])))

(defn emit-factor [ctx tokens]
  (parse-and-emit-binary-op ctx ::factor emit-prim tokens))

(defn emit-term [ctx tokens]
  (parse-and-emit-binary-op ctx ::term emit-factor tokens))

(defn emit-comparison [ctx tokens]
  (parse-and-emit-binary-op ctx ::comparison emit-term tokens))

(defn emit-equality [ctx tokens]
  (parse-and-emit-binary-op ctx ::equality emit-comparison tokens))

(defmulti emit-cmd (fn [_ cmd _] (:str cmd)))

(defmethod emit-cmd "LET" [ctx _ tokens]
  (let [[sym tokens] (expect-sym tokens)
        [val-pos ctx tokens] (emit-equality ctx tokens)
        kw-sym (keyword (:str sym))]
    [(if-let [stack-pos (get-in ctx [:env kw-sym :stack-pos])]
          (-> ctx
              (emit-get-local val-pos)
              (emit-set-local stack-pos))
          (let [[ctx stack-pos] (alloc-var ctx)
                ctx (emit-get-local ctx val-pos)
                ctx (emit-set-local ctx stack-pos)]
            (assoc-in ctx [:env kw-sym :stack-pos] stack-pos)))
     tokens]))

(defmethod emit-cmd "GOTO" [ctx _ tokens]
  (let [[{dst-label :str} tokens] (expect-num tokens)]
    [(emit-goto ctx dst-label) tokens]))

(defmethod emit-cmd "IF" [ctx _ tokens]
  (let [else-label (str (gensym "else__"))
        end-label (str (gensym "end__"))

        [pred-pos ctx tokens] (emit-equality ctx tokens)
        ctx (emit-branch-if-not ctx pred-pos else-label)

        ;; compile then
        [_ tokens] (expect-sym tokens "THEN")
        [cmd tokens] (expect-sym tokens)
        [ctx tokens] (emit-cmd ctx cmd tokens)
        ctx (emit-goto ctx end-label)

        ;; compile else
        ;; TODO make else optional
        ctx (emit-label ctx else-label)
        [_ tokens] (expect-sym tokens "ELSE")
        [cmd tokens] (expect-sym tokens)
        [ctx tokens] (emit-cmd ctx cmd tokens)
        ctx (emit-label ctx end-label)]
    [ctx tokens]))

(defmethod emit-cmd "PLOT" [ctx _ tokens]
  (let [[x-pos ctx tokens] (emit-equality ctx tokens)
        [y-pos ctx tokens] (emit-equality ctx tokens)
        [color-pos ctx tokens] (emit-equality ctx tokens)]
    [(emit-call ctx "draw_point" [x-pos y-pos color-pos])
     tokens]))

(defn get-loop-label [ctx var-name]
  (get-in ctx [:loops var-name]))

(defn get-for-start-label [ctx var-name]
  (let [label (or (get-loop-label ctx var-name)
                  (str (gensym (str "for_label_" var-name))))
        ctx (assoc-in ctx [:loops var-name] label)]
    [ctx label]))

(defn get-for-end-label [ctx var-name]
  (let [label (get-loop-label ctx var-name)
        _ (assert (some? label)
                  (str "loop with variable " var-name " doesn't exist"))
        label (str "end_" label)]
    [ctx label]))

(defmethod emit-cmd "FOR" [ctx _ tokens]
  (let [[{var-name :str} tokens] (expect-sym tokens) ;; verify
        [val-pos ctx tokens] (emit-equality ctx tokens)
        kw-var-name (keyword var-name)
        [ctx init-pos] (if-let [pos (get-stack-pos ctx kw-var-name)]
                         [ctx pos]
                         (alloc-var ctx))
        ctx (emit-get-local ctx val-pos)
        ctx (emit-set-local ctx init-pos)

        [_ tokens] (expect-sym tokens "TO")
        [end-pos ctx tokens] (emit-equality ctx tokens)
        [ctx sum-pos] (alloc-var ctx)
        ctx (assoc-in ctx [:env kw-var-name :stack-pos] init-pos)
        _ (assert (nil? (get-loop-label ctx var-name))
                  (str "Loop variable " var-name " already exist in a parent loop"))
        [ctx start-label] (get-for-start-label ctx var-name)
        ctx (emit-label ctx start-label)
        ctx (emit-binary-op ctx ::> sum-pos init-pos end-pos)
        [ctx end-label] (get-for-end-label ctx var-name)
        ctx (emit-branch-if ctx sum-pos end-label)]
    [ctx tokens]))

(defmethod emit-cmd "NEXT" [ctx _ tokens]
  (let [[{var-name :str} _] (expect-sym tokens)
        [var-pos ctx tokens] (deref-sym ctx tokens)
        [ctx start-label] (get-for-start-label ctx var-name)
        [ctx end-label] (get-for-end-label ctx var-name)
        ctx (assoc-in ctx [:loops var-name] nil)]
        [(-> ctx
             (emit-inc var-pos)
             (emit-goto start-label)
             (emit-label end-label))
         tokens]))

(defmethod emit-cmd :default [ctx cmd tokens]
  (throw (Exception. (str "invalid command: " cmd))))

(defn emit [{:keys [tokens] :as ctx}]
  (loop [ctx ctx
         [{:keys [type] label :str} & rem :as tokens] tokens]
    (if (empty? tokens)
      (-> ctx
          (emit-push 0)
          (emit-code "ret;"))
      (let [[ctx tokens]
            (if (= type ::num)
              [(emit-label ctx label) rem]
              [ctx tokens])
            
            [cmd tokens] (expect-sym tokens)
            [ctx tokens] (emit-cmd ctx cmd tokens)
            tokens (drop-while (fn [{:keys [type]}] (= type ::new-line)) tokens)]
        (recur ctx tokens)))))

(defn compile-src! [source]
  (let [ctx (->> source
                 slurp
                 (tokenize (create-ctx))
                 emit)]
    (spit "out.asm"
          (str (slurp "runtime.asm")
               (apply str (repeat (:var-count ctx) "push 0;\n"))
               (apply str (-> ctx :uvm-code))))))

(if (empty? *command-line-args*)
  (print "Provide a sourcefile as an argument. e.g. clj -M basic.clj source.clj")
  (compile-src! (first *command-line-args*)))

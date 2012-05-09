(ns squid.idiom.pipeline
  (:require [clojure.tools.macro :as m]))

;;
;; the "parsing" is cribbed heavily from condp
;;

(defn- max-arg-count [f]
  (apply max (map #(alength (.getParameterTypes %)) (.getDeclaredMethods (class f)))))

(defn pipeline-apply
  [f & args]
  (let [arity (max-arg-count f)
        count (count args)]
    (cond
     (>= arity count) (apply f args)
     (= arity 0)     (f)
     (= arity 1)     (f args)
     :else           (throw (IllegalArgumentException. "I have no idea what to do with that")))
    )
  )

(defmacro cond-pipeline
  "Takes an expression, and a set of clauses.
Each clause can take the form of either:

test-fn processing-fn

test-fn :>> processing-fn

Note :>> is an ordinary keyword.

The input to the first clause is 'expr'. Each clause after the
first one receives the output of the previous clause as its input.

For each clause, (test-fn input) is evaluated. If it returns
logical true, the clause is a match. If a binary clause matches, the
clause results in (processing-fn input).
If a ternary clause matches, its result-fn, which must be a unary function,
is called with the result of the predicate as its argument, the result of the clause
being the value of the clause, i.e. (processing-fn (pred input)).

If (test-fn expr) returns false, then the input is the result of the clause
and becomes the input into the next clause unchanged.

Again, the result of each clause is fed into the next clause as its input.  The
output of the final clause is the value of the entire cond-pipeline form.

A single final processing-fn can follow the clauses, and it will
be applied to the result of the penultimate clause.  That value will be returned
as the value of the cond-pipeline form."
  [expr & clauses]
  (let [gexpr (gensym "expr__")
        emit (fn emit [expr args]
               (let [[[a b c :as clause] more]
                     (split-at (if (= :>> (second args)) 3 2) args)
                     n (count clause)
                     ;; anaphora
                     itsym 'it          ; 'it' is the result of the predicate
                     eexpr 'input]      ; 'input' is the input to the clause
                 (cond
                  (= 0 n) expr
                  (= 1 n) `(~a ~expr)
                  (= 2 n) (emit `(let [~eexpr ~expr ~itsym (squid.idiom.pipeline/pipeline-apply ~a ~eexpr)]
                                   (if ~itsym (squid.idiom.pipeline/pipeline-apply ~b ~eexpr) ~eexpr)) more)
                  :else   (emit `(let [~eexpr ~expr ~itsym (~a ~eexpr)]
                                   (if ~itsym
                                     (squid.idiom.pipeline/pipeline-apply ~c ~itsym)
                                     ~eexpr)) more)
                  )))
        gres (gensym "res__")]
    `(let [~gexpr ~expr]
       ~(emit gexpr clauses))))


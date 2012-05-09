(ns squid.idiom.pipeline
  (:require [clojure.tools.macro :as m]))

;;
;; the "parsing" is cribbed heavily from condp
;;

(defn- max-arg-count [f]
  (apply max (map #(alength (.getParameterTypes %)) (.getDeclaredMethods (class f)))))

(defn pipeline-apply
  [f & args]
  (if (not (fn? f))
    f
    (let [arity (max-arg-count f)
          count (count args)]
      (cond
       (>= arity count) (apply f args)
       (= arity 0)     (f)
       (= arity 1)     (f args)
       :else           (throw (IllegalArgumentException. "I have no idea what to do with that"))))))

(defmacro cond-pipeline
  "Takes an expression, and a set of clauses.
Each clause can take one of these forms:

  test-fn processing-fn-or-result-expr

  test-fn :>> processing-fn-or-result-expr

In these clauses, the test-fn-or-expr is also known as the
LHS (left hand side) and the processing-fn-or-result-expr is
also known as the RHS (right hand side).  In general, the LHS
determines whether to pass through the clause's input into the
next clause unchanged or to pass the result of the RHS into the
next clause.

The following form is only available to the last clause in the body:

  processing-fn-or-result-expr

Note :>> is an ordinary keyword.

The input to the first clause is 'expr'. Each clause after the
first one receives the output of the previous clause as its input.

For each clause, the LHS is evaluated. If it returns
logical true, the clause is a match. If a binary clause matches, the
clause results in evaluation of the right-hand side.

If a ternary clause matches, its processing-fn, if a unary function,
is called with the result of the LHS as its argument, and the result
becomes the result of the clause, i.e. (processing-fn (pred input)).

If the LHS results in a non-true value, then the input is the result of the clause
and becomes the input into the next clause unchanged.

Again, the result of each clause is fed into the next clause as its input.  The
output of the final clause becomes the value of the entire cond-pipeline form.

LHS Evaluation:

The LHS of each clause may be either a predicate-function or
a predicate-expr.  If it is a predicate-function, then the predicate function
is applied to the clause's input.

If it is a predicate-expr, i.e. not a functional type, then the
predicate-expr is evaluated and used as the result of the LHS.


RHS Evaluation:

The RHS of each clause may be either a processing-function or
a result-expr.  If it is a processing-function, then the processing function
is applied to the processing-fn-input, which in a binary clause is the input
to the clause, and in a ternary clause is the result of (pred input).

If it is a result-expr, i.e. not a functional type, then the result-expr is
evaluated.

Environment and Anaphoric Symbols:

These symbols are available to each processing-fn-or-result-expr:

- 'input' - this is the input to the clause, available to LHS and RHS
- 'it'    - this is the result of (pred input) for the clause, and is only
            available to the RHS
Finally:

A single final processing-fn-or-result-expr can follow the clauses, and it will
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
                  (= 1 n) `(let [~eexpr ~expr] (squid.idiom.pipeline/pipeline-apply ~a ~eexpr))
                  (= 2 n) (emit `(let [~eexpr ~expr ~itsym (squid.idiom.pipeline/pipeline-apply ~a ~eexpr)]
                                   (if ~itsym (squid.idiom.pipeline/pipeline-apply ~b ~eexpr) ~eexpr)) more)
                  :else   (emit `(let [~eexpr ~expr ~itsym (squid.idiom.pipeline/pipeline-apply ~a ~eexpr)]
                                   (if ~itsym
                                     (squid.idiom.pipeline/pipeline-apply ~c ~itsym)
                                     ~eexpr)) more)
                  )))]
    `(let [~gexpr ~expr]
       ~(emit gexpr clauses))))

(defmacro compose-cond-pipeline
  "Like cond-pipeline, but omits the first 'input' argument and expands to a
form which returns a function which implements the pipeline specified in the
macro body.  This function can then be treated like any Clojure fn and be
applied to an argument once or more at any later time.

Example:

  ;; substitute a nil input with an empty vector; conj 99 onto
  ;; the result of the first clause
  ;;
  (def conj99 (compose-cond-pipeline
                            nil?     []
                            (conj input 99)))

  (conj99 nil)       ; => [99]
  (conj99 '(1 2 3))  ; => (99 1 2 3)
"
  [& clauses]
  (let [exprsym 'expr]                  ;do this to avoid namespacing
                                        ;of the symbol
    (list 'fn '[expr]
          `(cond-pipeline ~exprsym ~@clauses))))

(defn cond-pipeline-from-seq
  "A non-macro form of the cond-pipeline macro which, instead of evaluating
inline, composes a pipelined function from the [predicate result] clauses which
are evaluated in natural order.

If predicate is a function, then it is applied to the input of the clause, and
the result determines whether the clause yields the result of applying the result-fn
to the clauses's input or passes through the input unchanged (resp. true and false).

If predicate is an expression, then the value of that expression is used to
determine the disposition of the clause's input.  This is of limited value
except when the predicate is a simple true / false value which can be used
to insert transformations into the pipeline which are switched not by the
input data but always on or sensitive to some other available value (e.g. a
configuration setting).

As with cond-pipeline, the result-fn (RHS) may be either a function or
an expression which yields a non-functional value.  If the latter, the
value is used as the result of the clause.  This is most useful in having
the input conditionally replaced with some other data that need not be expressed
as a transformation of the input to the clause.  e.g., if the input is nil,
then use a default value.
 "
  [pred-fn-pairs]
  (apply comp
         (map (fn [[pred resfn]]
                (fn [input] (if (pipeline-apply pred input)
                             (pipeline-apply resfn input)
                             input))) (reverse pred-fn-pairs))))



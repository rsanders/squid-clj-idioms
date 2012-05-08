(ns squid.idiom.lazy-let
  (:require [clojure.tools.macro :as m]))

(defn- llsym
  [sym base]
  (symbol
   (str base "-" sym)))

(defn- delay-vals
  [binding-seq base]
  (let [pairs   (partition 2 binding-seq)]
    (into []
          (mapcat #(list (llsym (first %) base) (list 'delay (second %)))
                  pairs))))

(defn- deref-syms
  [binding-seq base]
  (vec (mapcat #(list % (list 'deref (llsym % base)))
          (take-nth 2 binding-seq))))

(defmacro lazy-let*
  "binding => binding-form init-expr

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. The difference from normal let* is that the init-expr is
delayed, i.e. not evaluated until the first actual evaluation of the
bound symbol. If the value of init-expr is not needed, it is not
evaluated.

Does not perform destructuring - see lazy-let."
  [bindings & body]  
  {:special-form true, :forms '[(lazy-let [bindings*] exprs*)],
   :pre [(vector? bindings)
         (even? (count bindings))]
   }

  (let [base-sym (gensym)]
  `(let* ~(delay-vals bindings base-sym)
         (m/symbol-macrolet ~(deref-syms bindings base-sym)
                          ~@body
                          ))))

(defmacro lazy-let
  "binding => binding-form init-expr

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. The difference from normal let is that the init-expr is
delayed, i.e. not evaluated until the first actual evaluation of the
bound symbol. If the value of init-expr is not needed, it is not
evaluated.

Does perform destructuring, unlike lazy-let*."
  [bindings & body]  
  {:special-form true, :forms '[(lazy-let [bindings*] exprs*)],
   :pre [(vector? bindings)
         (even? (count bindings))]
   }
  (let [dbindings (destructure bindings)]
    (if (= (count dbindings) 2)
      `(lazy-let* [~(first dbindings) ~(second dbindings)]
                  ~@body
                  )
      `(lazy-let* [~(first dbindings) ~(second dbindings)]
                  (lazy-let ~(vec (drop 2 dbindings))
                            ~@body))
      )))


;; (m/mexpand-all '(lazy-let3 [a 12 b 13 [d e f] (list 1 b 3)]
;;                         (list a b d e f)))
;; (let* [G__4164-a (new clojure.lang.Delay (fn* ([] 12)))] (do (let* [G__4165-b (new clojure.lang.Delay (fn* ([] 13)))] (do (let* [G__4166-vec__4163 (new clojure.lang.Delay (fn* ([] (list 1 (deref G__4165-b) 3))))] (do (let* [G__4167-d (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__4166-vec__4163) 0 nil))))] (do (let* [G__4168-e (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__4166-vec__4163) 1 nil))))] (do (let* [G__4169-f (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__4166-vec__4163) 2 nil))))] (do (list (deref G__4164-a) (deref G__4165-b) (deref G__4167-d) (deref G__4168-e) (deref G__4169-f))))))))))))))


;; (lazy-let3 [a 12 b (* 9 9) [d e f] (list 1 b 3)]
;;            (list a b d e f))
;; (12 81 1 81 3)


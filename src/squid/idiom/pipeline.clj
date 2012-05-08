(ns squid.idiom.pipeline
  (:require [clojure.tools.macro :as m]))

(comment
  (defn is-fooable? [list]
    (odd? (count list)))

  (defn do-foo [list]
    (map incr list))

  (def is-barable? (comp not is-fooable?))

  ;; apply a series of conditional transformations to an input 
  (cond-pipeline [1 2 3 4 5]
                 ;; assuming is-fooable? returns true, maps do-foo across [1 2 3 4 5]
                 is-fooable?  do-foo

                 ;; if list doesn't satisfy is-barable, do nothing
                 is-barable?  do-bar
                 
                 ;; doubles every element in list [2 3 4 5 6]
                 is-addable?  (partial * 2)

                 ;; takes list [3 4 5 6] and returns [4 6]
                 rest         #(filter even? it)

                 ;; takes [4 6] and returns [6 4]
                 :finally     reverse)
  )

(comment
  ;; like let, except it lazily evaluates RHS only when needed

  (lazy-let* [foo  (do (println "f") (Thread/sleep 10000) 5)
             bar  (do (println "b") 5)]
            (+ bar 5))

  ;; will print "b" and return 10; never sleeps or prints "f"
  )

(defn ^:private llsym
  [sym base]
  (symbol
   (str base "-" sym)))

(defn ^:private delay-vals
  [binding-seq base]
  (let [pairs   (partition 2 binding-seq)]
    (into []
          (mapcat #(list (llsym (first %) base) (list 'delay (second %)))
                  pairs))))

(defn ^:private deref-syms
  [binding-seq base]
  (vec (mapcat #(list % (list 'deref (llsym % base)))
          (take-nth 2 binding-seq))))

(defn ^:private destructure-gensym?
  [sym]
  (condp = (.substring (str sym) 0 5)
    "vec__" true
    "map__" true
    false
    ))

(defmacro lazy-let*
  "binding => binding-form init-expr

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. The difference from normal let* is that the init-expr is
delayed, i.e. not evaluated until the first actual evaluation of the
bound symbol. If the value of init-expr is not needed, it is not
evaluated.

Does not perform destructuring."
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
therein. The difference from normal let* is that the init-expr is
delayed, i.e. not evaluated until the first actual evaluation of the
bound symbol. If the value of init-expr is not needed, it is not
evaluated.

Does not perform destructuring."
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

(m/mexpand-all '(lazy-let3 [a 12 b 13 [d e f] (list 1 b 3)]
                        (list a b d e f)))
(let* [G__4164-a (new clojure.lang.Delay (fn* ([] 12)))] (do (let* [G__4165-b (new clojure.lang.Delay (fn* ([] 13)))] (do (let* [G__4166-vec__4163 (new clojure.lang.Delay (fn* ([] (list 1 (deref G__4165-b) 3))))] (do (let* [G__4167-d (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__4166-vec__4163) 0 nil))))] (do (let* [G__4168-e (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__4166-vec__4163) 1 nil))))] (do (let* [G__4169-f (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__4166-vec__4163) 2 nil))))] (do (list (deref G__4164-a) (deref G__4165-b) (deref G__4167-d) (deref G__4168-e) (deref G__4169-f))))))))))))))


(lazy-let3 [a 12 b 13 [d e f] (list 1 b 3)]
           (list a b d e f))
(12 13 1 13 3)

(destructure '[a 12 b 13 [d e f] (list 1 b 3)])
->
[a 12 b 13
 vec__3571 (list 1 b 3)
 d (clojure.core/nth vec__3571 0 nil)
 e (clojure.core/nth vec__3571 1 nil)
 f (clojure.core/nth vec__3571 2 nil
                     )]

;;

(m/symbol-macrolet
 [d-a (delay 12)
  d-b (delay 13)
  d-vec__3571 (delay (list 1 b 3))
  d-e (delay (clojure.core/nth vec__3571 1 nil))
  a (deref d-a)
  b (deref d-b)
  vec__3571 (deref d-vec__3571)
  e (deref d-e)]
 (list a b e)
 )
;; -> (12 13 13)

(m/mexpand '(m/symbol-macrolet
             [d-a (delay 12)
              d-b (delay 13)
              d-vec__3571 (delay (list 1 b 3))
              d-e (delay (clojure.core/nth vec__3571 1 nil))
              a (deref d-a)
              b (deref d-b)
              vec__3571 (deref d-vec__3571)
              e (deref d-e)]
             (list a b e)
))

(do (list
     (deref (new clojure.lang.Delay (fn* ([] 12))))
     (deref (new clojure.lang.Delay (fn* ([] 13))))
     (deref (new clojure.lang.Delay (fn* ([]
                                            (clojure.core/nth (deref
                                                               (new clojure.lang.Delay
                                                                    (fn* ([] (list 1 (deref (new clojure.lang.Delay (fn* ([] 13)))) 3))))) 1 nil)))))))



;;

(destructure '[a 12 b 13 [d e f] (list 1 b 3)])
->
[a 12
 b 13
 vec__3571 (list 1 b 3)
 d  (clojure.core/nth vec__3571 0 nil)
 e  (clojure.core/nth vec__3571 1 nil)
 f  (clojure.core/nth vec__3571 2 nil)
 ]

(lazy-let [a 12]
          (lazy-let [b 13]
                    (lazy-let [vec__3571 (list 1 b 3)]
                              (lazy-let [d  (clojure.core/nth vec__3571 0 nil)]
                                        (lazy-let [e  (clojure.core/nth vec__3571 1 nil)]
                                                  (list a b d e))))))
(12 13 1 13)

(m/mexpand-all
 '(lazy-let [a 12]
(lazy-let [b 13]
(lazy-let [vec__3571 (list 1 b 3)]
(lazy-let [d  (clojure.core/nth vec__3571 0 nil)]
(lazy-let [e  (clojure.core/nth vec__3571 1 nil)]
(list a b d e))))))
)

(let* [G__3804-G__3805 (new clojure.lang.Delay (fn* ([] 12)))] (do (let* [G__3806-a (new clojure.lang.Delay (fn* ([] (deref G__3804-G__3805))))] (do (let* [G__3807-G__3808 (new clojure.lang.Delay (fn* ([] 13)))] (do (let* [G__3809-b (new clojure.lang.Delay (fn* ([] (deref G__3807-G__3808))))] (do (let* [G__3810-G__3811 (new clojure.lang.Delay (fn* ([] (list 1 (deref G__3809-b) 3))))] (do (let* [G__3812-vec__3571 (new clojure.lang.Delay (fn* ([] (deref G__3810-G__3811))))] (do (let* [G__3813-G__3814 (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__3812-vec__3571) 0 nil))))] (do (let* [G__3815-d (new clojure.lang.Delay (fn* ([] (deref G__3813-G__3814))))] (do (let* [G__3816-G__3817 (new clojure.lang.Delay (fn* ([] (clojure.core/nth (deref G__3812-vec__3571) 1 nil))))] (do (let* [G__3818-e (new clojure.lang.Delay (fn* ([] (deref G__3816-G__3817))))] (do (list (deref G__3806-a) (deref G__3809-b) (deref G__3815-d) (deref G__3818-e))))))))))))))))))))))

;;

(m/mexpand '(m/symbol-macrolet [a 12 b (+ a 2)]
                               b a))
;; -> (do (+ 12 2))



;;

(m/mexpand
 '(lazy-let
   [vec__3378 (list 1 2 3)]
   (lazy-let
    [a 12
     b 13
     d (clojure.core/nth vec__3378 0 nil)
     e (clojure.core/nth vec__3378 1 nil)
     f (clojure.core/nth vec__3378 2 nil)]
    e)))

(let*
  [G__3467-G__3468 (delay (list 1 2 3))]
  (clojure.tools.macro/symbol-macrolet
   [G__3468 (deref G__3467-G__3468)]
   (squid.idiom.pipeline/lazy-let* [vec__3378 G__3468]
                                   (lazy-let [a 12
                                              b 13 
                                              d (clojure.core/nth vec__3378 0 nil)
                                              e (clojure.core/nth vec__3378 1 nil)
                                              f (clojure.core/nth vec__3378 2 nil)]
                                             e))))
;; -> 2

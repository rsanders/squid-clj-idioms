(ns squid.idiom.lazy-let-test
  (:use clojure.test
        squid.idiom.lazy-let))

(def ^:dynamic counter)

(deftest simple-let
  (testing "simple lazy-let*"
    (is (=
         (lazy-let* [a 1 b 2]
                   (list a b))
         '(1 2)
         ))
    ))

;; this uses an atom to count side-effects
;;
;; explanation of final vector in the result list:
;;
;;   first ref to atomval2 should resolve (and cache) base atom val of 0
;;   any use of @counter will return current atom val, at this point
;;       in time still 0
;;   first ref of atomval1 will inc counter and eval to 1
;;   second and subsequent refs of atomval1 will return cached value of 1
;;   next ref of @counter returns current counter val of 1
;;   atomval2 should still return cached value of0
;;
(deftest lazy-let-destructuring
  (binding [counter (atom 0)]
    (testing "lazy-let with destructuring and simple backref"
      (reset! counter 0)
      (is (=
           (lazy-let [a 12
                      b (* 9 9)
                      atomval1 (swap! counter inc)
                      atomval2 (deref counter)
                      [d e f :as vec] (list 1 b 3)]
                     (list a b d e f vec [atomval2 @counter atomval1 atomval1 @counter atomval2]))
           '(12 81 1 81 3 [1 81 3]
                [0 0 1 1 1 0])
           ))
      )))

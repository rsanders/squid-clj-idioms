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
                     (list a b d e f vec [@counter atomval1 atomval1 @counter atomval2]))
           '(12 81 1 81 3 [1 81 3]
                [0 1 1 1 1])
           ))
      )))

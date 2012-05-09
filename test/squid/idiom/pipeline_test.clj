(ns squid.idiom.pipeline-test
  (:use clojure.test
        squid.idiom.pipeline))

(defn is-fooable? [list]
  (odd? (count list)))

(defn do-foo [list]
  (map inc list))

(defn do-bar [list]
  (assert false "shouldn't do-bar"))

(def is-barable? (comp not is-fooable?))

(defn is-addable? [list] true)

(deftest simple-cond-pipelines
  (testing "empty pipeline"
    (is (= [1 2 3 4 5]
           (cond-pipeline [1 2 3 4 5]))))

  (testing "one element (final processing-fn) pipeline"
    (is (= [5 4 3 2 1]
           (cond-pipeline [1 2 3 4 5]
                          reverse))))

  (testing "one binary clause pipeline"
    (is (= [2 3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          is-fooable?  do-foo))))

  (testing "one ternary clause pipeline"
    (is (= [3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          rest :>> do-foo))))
  )

(deftest binding-guarantees
  (testing "single eval of input for binary clause"
    (let [counter (atom 0)]
      (cond-pipeline (swap! counter inc)
                     identity identity)
      (is (= 1 @counter))))

  (testing "single eval of input for ternary clause"
    (let [counter (atom 0)]
      (cond-pipeline (swap! counter inc)
                     identity :>> identity)
      (is (= 1 @counter)))))
  

(deftest simple-cond-pipelines
  (testing "empty pipeline"
    (is (= [1 2 3 4 5]
           (cond-pipeline [1 2 3 4 5]))))

  (testing "one element (final processing-fn) pipeline"
    (is (= [5 4 3 2 1]
           (cond-pipeline [1 2 3 4 5]
                          reverse))))

  (testing "one binary clause pipeline"
    (is (= [2 3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          is-fooable?  do-foo))))

  (testing "one ternary clause pipeline"
    (is (= [3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          rest :>> do-foo))))
  )

(deftest test-all-features-pipeline
  (testing "the initial five-part initial pipeline"
    (is (= [18 12]
           (cond-pipeline [1 2 3 4 5]
                          ;; assuming is-fooable? returns true, maps do-foo across [1 2 3 4 5]
                          is-fooable?  do-foo
                          
                          ;; if list doesn't satisfy is-barable, do nothing
                          is-barable?  do-bar

                          ;; triples every element in list [2 3 4 5 6]
                          is-addable?  (partial map (partial * 3))

                          ;; takes list [6 9 12 15 18], takes
                          ;; rest of it [9 12 15 18] as input to action,
                          ;; and returns [12 18]

                          rest      :>>   #(filter even? %)
                          
                          ;; equivalent to:
                          ;; rest     (fn [_] (filter even? it))

                          ;; takes [12 18] and returns [18 12]
                          reverse)))))


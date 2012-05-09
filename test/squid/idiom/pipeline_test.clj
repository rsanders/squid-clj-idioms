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
  

(deftest anaphora-for-processor-fns
  (testing "one element (final processing-fn) pipeline taking 0 args"
    (is (= [:a :b :c]
           (cond-pipeline [1 2 3 4 5]
                          #(list :a :b :c)))))

  (testing "one element (final processing-fn) pipeline using 'input' anaphora and taking 0 args"
    (is (= [2 3 4 5]
           (cond-pipeline [1 2 3 4 5]
                          #(rest input)))))
  
  (testing "one binary clause pipeline w/processing-fn using 'it' and taking 0 args"
    (is (= 6
           (cond-pipeline [1 2 3 4 5]
                          last  #(inc it)))))

  (testing "one ternary clause pipeline for processing-fn taking 0 args"
    (is (= [3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          rest :>> #(do-foo it)))))
  )

(deftest anaphora-for-processor-exprs
  (testing "one element (final result-expr) pipeline"
    (is (= [:a :b :c]
           (cond-pipeline [1 2 3 4 5]
                          (list :a :b :c)))))

  (testing "one element (final result-expr) pipeline using 'input' anaphora"
    (is (= [2 3 4 5]
           (cond-pipeline [1 2 3 4 5]
                          (rest input)))))

  (testing "one binary clause pipeline w/result-expr using 'it'"
    (is (= 6
           (cond-pipeline [1 2 3 4 5]
                          last  (inc it))))

    ;; should return input as conditional is false
    ;; should NOT evaluate result-expr, which in this case would error
    (is (= [1 2 3 4 5]
           (cond-pipeline [1 2 3 4 5]
                          (comp next empty) (inc it))))
    )

  (testing "one ternary clause pipeline for result-expr"
    (is (= [3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          rest :>> (do-foo it))))

    ;; should return input because conditional is false (nil)
    ;; should NOT evaluate result-expr, which in this case would error
    (is (= [1 2 3 4 5]
           (cond-pipeline [1 2 3 4 5]
                          (comp next empty) :>> (do-foo it)))))
  )


(deftest compound-cond-pipelines
  (testing "one clause + final processing-fn pipeline"
    (is (= [5 4 3 2 1]
           (cond-pipeline [1 2 3 4 5]
                          empty?   [:a :b]
                          reverse))))

  (testing "two clause pipeline"
    (is (= [3 4 5 6]
           (cond-pipeline [1 2 3 4 5]
                          is-fooable?  do-foo
                          seq          rest
                          ))))

  (testing "two clause pipeline skipping first clause"
    (is (= [2 4]
           (cond-pipeline [1 2 3 4 5]
                          empty?   (partial conj :a)
                          vector?  (partial filter even?)))))

  (testing "two clause pipeline skipping both clauses"
    (is (= [1 2 3 4 5]
           (cond-pipeline [1 2 3 4 5]
                          empty?   (partial conj :a)
                          list?  (partial filter even?)))))
  )


(deftest test-all-features-pipeline
  (testing "the initial five-part initial pipeline"
    (is (= [18 12]
           (cond-pipeline [1 2 3 4 5]
                          ;; assuming is-fooable? returns true, maps do-foo across [1 2 3 4 5]
                          is-fooable?  do-foo
                          
                          ;; if list doesn't satisfy is-barable, do nothing
                          is-barable?  #(do-bar input)

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

;;;
;;; functional equivalent
;;;

(deftest test-simple-pipeline-fn
  (testing "0 clause pipeline function constants"
    (is (= [1 2 3 4 5]
           ((cond-pipeline-fn [])
            [1 2 3 4 5]))))

  (testing "1 clause pipeline-fn"
    (is (= [1 2 3]
           ((cond-pipeline-fn [[(complement empty?) (partial take 3)]])
            [1 2 3 4 5]))))

  (testing "1 clause pipeline-fn w/expr predicate"
    (is (= [1 2 3]
           ((cond-pipeline-fn [[true (partial take 3)]])
            [1 2 3 4 5]))))

  (testing "1 clause pipeline-fn w/expr predicate (false"
    (is (= [1 2 3 4 5]
           ((cond-pipeline-fn [[false (partial take 3)]])
            [1 2 3 4 5]))))

  (testing "1 clause pipeline-fn w/expr result-fn"
    (is (= :empty
           ((cond-pipeline-fn [[empty? :empty]])
            []))))
  )

(deftest test-fuller-pipeline-fn
  (testing "4 clause pipeline function with a couple of constants"
    (is (= [4 3 2 1]
           ((cond-pipeline-fn [[seq reverse]
                               [#(odd? (count %)) rest]
                               [false :a]
                               [true identity]])
            [1 2 3 4 5]))))
  )
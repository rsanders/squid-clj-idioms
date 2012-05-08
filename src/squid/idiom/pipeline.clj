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

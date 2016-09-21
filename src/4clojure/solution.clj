(ns solutions)


;; http://www.4clojure.com/problem/16
;; Hello World
;; Write a function which returns a personalized greeting.
(fn [person] (str "Hello, " person "!"))

;; http://www.4clojure.com/problem/19
;; Last Element
;; Write a function which returns the last element in a sequence.
#(nth % (- (count %) 1))


;; http://www.4clojure.com/problem/20
;; Penultimate Element
;; Write a function which returns the second to last element from a sequence.
#(nth % (- (count %) 2))

;; http://www.4clojure.com/problem/21
;; Nth Element
;; Write a function which returns the Nth element from a sequence.
(fn
  [coll index]
  (loop [remaining coll
         collected []]
    (if (= (count collected) index)
      (first remaining)
      (recur (rest remaining) (conj collected (first remaining))))))

;; http://www.4clojure.com/problem/22
;; Count a Sequence
;; Write a function which returns the total number of elements in a sequence.
#(.size (seq %))

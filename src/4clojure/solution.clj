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


;; http://www.4clojure.com/problem/23
;; Reverse a Sequence
;; Write a function which reverses a sequence.
(fn
  [coll]
  (loop [remaining coll
         reversed ()]
    (if (empty? remaining)
      reversed
      (recur (rest remaining) (conj reversed (first remaining))))))


;; http://www.4clojure.com/problem/27
;; Palindrome Detector
;; Write a function which returns true if the given sequence is a palindrome.
(fn [original] (= (seq original) (reverse (seq original))))


;; http://www.4clojure.com/problem/26
;; Fibonacci Sequence
;; Write a function which returns the first X fibonacci numbers.
(fn [x]
  (take x
        ((fn fib [a b]
           (cons a (lazy-seq (fib b (+ a b)))))
         1 1)))


;; http://www.4clojure.com/problem/38
;; Maximum value
;; Write a function which takes a variable number of parameters and returns
;; the maximum value.
(fn max-value
  [& more]
  (cond (empty? more)
        more

        (empty? (rest more))
        (first more)

        (> (first more) (second more))
        (recur ( cons (first more) (rest (rest more))))

        :else
        (recur (rest more))))

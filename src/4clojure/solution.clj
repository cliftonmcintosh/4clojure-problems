(ns solutions)


;; collected in one file after solving most of the problems
;; in 4clojure. Done in order "Times Solved" by starting with
;; the most-often solved problem and moving down towards less-
;; often solved problems.


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


;; http://www.4clojure.com/problem/72
;; Rearranging Code: ->>
;; The ->> macro threads an expression x through a variable number of forms.
;; First, x is inserted as the last item in the first form, making a list of it
;; if it is not a list already. Then the first form is inserted as the last item
;; in the second form, making a list of that form if necessary. This process
;; continues for all the forms. Using ->> can sometimes make your code more
;; readable.
#(reduce + %)


;; http://www.4clojure.com/problem/29
;; Get the Caps
;; Write a function which takes a string and returns a new string containing
;; only the capital letters.
(fn
  [input]
  (apply str (re-seq #"[A-Z]" input)))


;; http://www.4clojure.com/problem/134
;; A nil key
;; Write a function which, given a key and map, returns true
;; iff (http://en.wikipedia.org/wiki/If_and_only_if) the map contains an entry
;; with that key and its value is nil.
(fn [k coll] (and (contains? coll k) (nil? (k coll))))


;; http://www.4clojure.com/problem/32
;; Duplicate a Sequence
;; Write a function which duplicates each element of a sequence.
(fn dup-seq
  ([input] (dup-seq input []))
  ([input accum]
   (if (empty? input)
     accum
     (recur (rest input) (conj accum (first input) (first input))))))


;; http://www.4clojure.com/problem/162
;; Implement range
;; Write a function which creates a list of all integers in a given range.
(fn my-range
  ([start end] (my-range start end []))
  ([start end accum]
   (if (>= start end)
     accum
     (recur (inc start) end (conj accum start)))))


;; http://www.4clojure.com/problem/30
;; Compress a Sequence
;; Write a function which removes consecutive duplicates from a sequence.
(fn compress
  [input]
  (map first (partition-by identity input)))


;; http://www.4clojure.com/problem/42
;; Factorial Fun
;; Write a function which calculates factorials.
(fn factorial
  [x]
  (reduce * (range 1 (+ x 1))))


;; http://www.4clojure.com/problem/39
;; Interleave Two Seqs
;; Write a function which takes two sequences and returns the first item
;; from each, then the second item from each, then the third, etc.
(fn my-interleave
  [x y]
  (flatten (map #(conj [] %1 %2) x y)))


;; http://www.4clojure.com/problem/28
;; Flatten a Sequence
;; Write a function which flattens a sequence.
(fn my-flatten
  [coll]
  (let [left (first coll)
        right (next coll)]
    (concat
     (if (sequential? left)
       (my-flatten left)
       [left])
     (when (sequential? right)
       (my-flatten right)))))


;; http://www.4clojure.com/problem/33
;; Replicate a Sequence
;; Write a function which replicates each element of a sequence a variable
;; number of times.
(fn rep-seq
  ([input num-times] (rep-seq input [] num-times))
  ([input accum num-times]
   (if (empty? input)
     accum
     (recur (rest input)
            (apply conj accum (repeat num-times (first input))) num-times))))

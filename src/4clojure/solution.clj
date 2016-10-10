(ns solutions)

;; 4clojure id: cliffmcintosh


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
(fn replicate' [coll x]
  (reduce (fn [a c] (apply conj a (repeat x c)))
          []
          coll))

;; http://www.4clojure.com/problem/52
;; Intro to Destructuring
;; Let bindings and function parameter lists support destructuring.
(conj [] c e)


;; http://www.4clojure.com/problem/40
;; Interpose a Seq
;; Write a function which separates the items of a sequence by an arbitrary
;; value.
(fn my-interpose
  [interposer coll]
  (loop [x interposer
         xs coll
         accum []]
    (cond
      (empty? xs) accum
      (= (count xs) 1) (conj accum (first xs))
      :else (recur x (rest xs) (conj accum (first xs) x)))))


;; http://www.4clojure.com/problem/31
;; Pack a Sequence
;; Write a function which packs consecutive duplicates into sub-lists.
(fn [input]
  (partition-by identity input))


;; http://www.4clojure.com/problem/156
;; When retrieving values from a map, you can specify default values in case the
;; key is not found:
;;
;; (= 2 (:foo {:bar 0, :baz 1} 2))
;;
;; However, what if you want the map itself to contain the default values?
;; Write a function which takes a default value and a sequence of keys and
;; constructs a map.
(fn map-defaults
  [value the-seq]
  (zipmap the-seq (take (count the-seq) (iterate identity value))))


;; http://www.4clojure.com/problem/41
;; Drop Every Nth Item
;; Write a function which drops every Nth item from a sequence.
(fn drop-every-nth
  [original index]
  (let [indexed-items (->> original
                           (map-indexed vector)
                           (map #(conj [] (inc (first %)) (second %))))]
    (->> indexed-items
         (filter (fn [input]
                   (not= 0 (rem (first input) index))))
         (reduce #(conj %1 (take-last 1 %2)) [])
         (flatten))))


;; http://www.4clojure.com/problem/49
;; Split a sequence
;; Write a function which will split a sequence into two parts.
#(vector (take %1 %2) (drop %1 %2))


;; http://www.4clojure.com/problem/46
;; Flipping out
;; Write a higher-order function which flips the order of the arguments of an
;; input function.
(fn [f] #(f %2 %1))


;; http://www.4clojure.com/problem/83
;; A Half-Truth
;; Write a function which takes a variable number of booleans. Your function
;; should return true if some of the parameters are true, but not all of the
;; parameters are true. Otherwise your function should return false.
(fn half-truth
  [& xs]
  (if (and (some true? xs) (some false? xs))
    true
    false))


;; http://www.4clojure.com/problem/61
;; Map Construction
;; Write a function which takes a vector of keys and a vector of values and
;; constructs a map from them.
(fn construct-map
  [x y]
  (apply merge (map #(assoc {} %1 %2) x y)))


;; http://www.4clojure.com/problem/66
;; Greatest Common Divisor
;; Given two integers, write a function which returns the greatest common
;; divisor.
(fn divisor
  [x y]
  (let [smaller
        (if (< x y) x y)
        larger
        (if (= x smaller) y x)
        largest-possible-divisor
        (if (>= (/ larger smaller) 2)
          smaller
          (if (> smaller 0) (int (float (/ smaller 2))) 0))
        possibles
        (reverse (range (inc largest-possible-divisor)))
        finder
        (fn [l s candidates]
          (let [to-check (first candidates)
                more (rest candidates)]
            (if (and (zero? (rem l to-check))
                     (zero? (rem s to-check)))
              to-check
              (recur l s more))))]
    (finder larger smaller possibles)))


;; http://www.4clojure.com/problem/44
;; Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
(fn rotate-sequence
  [pivot coll]
  (let [length (count coll)
        positive-pivot? (> pivot 0)
        new-pivot (if positive-pivot?
                    (if (>= length pivot) pivot (- pivot length))
                    (if (>= length (Math/abs pivot))
                      (+ length pivot)
                      (+ pivot (* 2 length))))]
    (concat (drop new-pivot coll) (take new-pivot coll))))


;; http://www.4clojure.com/problem/43
;; Reverse Interleave
;; Write a function which reverses the interleave process into x number of
;; subsequences.
(fn rev-interleave
  [xs point]
  (let [partitioned (partition point xs)
        helper (fn [coll accum]
                 (if (some empty? coll)
                   accum
                   (recur (map rest coll) (conj accum (map first coll)))))]
    (helper partitioned [])))


;; http://www.4clojure.com/problem/50
;; Split by Type
;; Write a function which takes a sequence consisting of items with different
;; types and splits them up into a set of homogeneous sub-sequences. The
;; internal order of each sub-sequence should be maintained, but the
;; sub-sequences themselves can be returned in any order (this is why 'set' is
;; used in the test cases).
(fn split-by-type [xs]
  (set (map second (group-by class xs))))


;; http://www.4clojure.com/problem/81
;; Set Intersection
;; Write a function which returns the intersection of two sets. The intersection
;; is the sub-set of items that each set has in common.
(fn [xs ys] (into #{} (filter #(contains? xs %) ys)))

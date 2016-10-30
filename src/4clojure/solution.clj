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
  (reduce (fn [a c] (concat a (repeat x c))) [] coll))


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
(fn drop-nth [coll n]
  (mapcat (fn [item] (if (= n (count item)) (butlast item) item))
          (partition-all n coll)))

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


;; http://www.4clojure.com/problem/166
;; Comparisons
;; For any orderable data type it's possible to derive all of the basic
;; comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation
;; (any operator but = or ≠ will work). Write a function that takes three
;; arguments, a less than operator for the data and two items to compare. The
;; function should return a keyword describing the relationship between the two
;; items. The keywords for the relationship between x and y are as follows:
;; x = y → :eq
;; x > y → :gt
;; x < y → :lt
(fn comparison [op x y]
  (cond
    (and (not (op x y)) (not (op y x))) :eq
    (not (op x y)) :gt
    :else :lt))


;; http://www.4clojure.com/problem/62
;; Re-implement Iterate
;; Given a side-effect free function f and an initial value x write a function
;; which returns an infinite lazy sequence of x, (f x), (f (f x)),
;; (f (f (f x))), etc.
(fn my-iter [f x] (lazy-seq (cons x (my-iter f (f x)))))


;; http://www.4clojure.com/problem/107
;; Simple closures
;; Lexical scope and first-class functions are two of the most basic building
;; blocks of a functional language like Cloture. When you combine the two
;; together, you get something very powerful called lexical closures.
;; With these, you can exercise a great deal of control over the lifetime of
;; your local bindings, saving their values for use later, long after the code
;; you're running now has finished
;;
;; It can be hard to follow in the abstract, so let's build a simple closure.
;; Given a positive integer n, return a function (f x) which computes xn.
;; Observe that the effect of this is to preserve the value of n for use outside
;; the scope in which it is defined.
(fn power [n]
  (fn [x]
    (reduce * (repeat n x))))


;; http://www.4clojure.com/problem/90
;; Write a function which calculates the Cartesian product of two sets.
;; http://en.wikipedia.org/wiki/Cartesian_product
(fn cart [x y]
  (set (for [value x
             suit y]
         [value suit])))


;; http://www.4clojure.com/problem/99
;; Product Digits
;; Write a function which multiplies two numbers and returns the result as a
;; sequence of its digits.
(fn product-digits
  [x y]
  (->> (* x y)
       str
       seq
       (map str)
       (map read-string)))


;; http://www.4clojure.com/problem/63
;; Group a Sequence
;; Given a function f and a sequence s, write a function which returns a map.
;; The keys should be the values of f applied to each item in s. The value at
;; each key should be a vector of corresponding items in the order they appear
;; in s.
(fn grpr-2 [f s]
  (reduce (fn [accum item]
            (let [r (f item)
                  items-for-key (get accum r)]
              (assoc accum r (if items-for-key
                               (conj items-for-key item) [item])))) {} s))


;; http://www.4clojure.com/problem/88
;; Symmetric Difference
;; Write a function which returns the symmetric difference of two sets. The
;; symmetric difference is the set of items belonging to one but not both of the
;; two sets.
(fn diffs [s1 s2]
  (set (concat
        (clojure.set/difference s1 s2)
        (clojure.set/difference s2 s1))))


;; http://www.4clojure.com/problem/143
;; dot product
;; Create a function that computes the dot product of two sequences. You may
;; assume that the vectors will have the same length.
(fn dot [xs ys]
  (reduce + (map * xs ys)))


;; http://www.4clojure.com/problem/122
;; Read a binary number
;; Convert a binary number, provided in the form of a string, to its numerical
;; value.
#(Integer/parseInt % 2)

;; http://www.4clojure.com/problem/135
;; Infix Calculator
;; Your friend Joe is always whining about Lisps using the prefix notation for
;; math. Show him how you could easily write a function that does math using the
;; infix notation. Is your favorite language that flexible, Joe? Write a
;; function that accepts a variable length mathematical expression consisting of
;; numbers and the operations +, -, *, and /. Assume a simple calculator that
;; does not do precedence and instead just calculates left to right.
(fn nfix [& [seed & coll]]
  (reduce (fn [v [op numb]]
            (op v numb))
          seed (partition 2 coll)))


;; http://www.4clojure.com/problem/157
;; Indexing Sequences
;; Transform a sequence into a sequence of pairs containing the original
;; elements along with their index.
(fn idx-seq [coll]
  (map-indexed (fn [idx item] [item idx]) coll))


;; http://www.4clojure.com/problem/97
;; Pascal's Triangle
;; http://en.wikipedia.org/wiki/Pascal%27s_triangle
;; Pascal's triangle is a triangle of numbers computed using the following
;; rules:
;;
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the
;; row above, and adding a 1 to the beginning and end of the row.
;;
;; Write a function which returns the nth row of Pascal's Triangle.
(fn pascal [y]
  (let [n (dec y)
        factorial (fn [x] (reduce * (range 1 (inc x))))
        n-choose-r (fn [r] (/ (factorial n)
                              (* (factorial r)
                                 (factorial (- n r)))))]
    (conj (into [] (map n-choose-r (range n))) 1)))


;; http://www.4clojure.com/problem/118
;; Re-implement Map
;; Map is one of the core elements of a functional programming language. Given a
;; function f and an input sequence s, return a lazy sequence of (f x) for each
;; element x in s.
(fn my-map [f xs]
  (if (empty? xs) xs
      (cons (f (first xs)) (lazy-seq (my-map f (rest xs))))))


;; http://www.4clojure.com/problem/95
;; To Tree, or not to Tree
;; Write a predicate which checks whether or not a given sequence represents a
;; binary tree. Each node in the tree must have a value, a left child, and a
;; right child.
(fn is-tree? [xs]
  (and (sequential? xs)
       (= 3 (count xs))
       (not (sequential? (first xs)))
       (not (nil? (first xs)))
       (or (nil? (second xs)) (is-tree? (second xs)))
       (or (nil? (last xs)) (is-tree? (last xs)))))


;; http://www.4clojure.com/problem/120
;; Sum of square of digits
;; Write a function which takes a collection of integers as an argument. Return
;; the count of how many elements are smaller than the sum of their squared
;; component digits. For example: 10 is larger than 1 squared plus 0 squared;
;; whereas 15 is smaller than 1 squared plus 5 squared.
(fn sum-of-squares [coll]
  (letfn [(component-squares [a-num]
            (reduce (fn [c n] (+ c (* n n)))
                    0 (map #(Character/getNumericValue %) (str a-num))))
          (accum-smallers [c n]
            (if (< n (component-squares n))
              (inc c)
              c))]
    (reduce accum-smallers
            0 coll)))


;; http://www.4clojure.com/problem/128
;; Recognize Playing Cards
;; A standard American deck of playing cards has four suits - spades, hearts,
;; diamonds, and clubs - and thirteen cards in each suit. Two is the lowest
;; rank, followed by other integers up to ten; then the jack, queen, king, and
;; ace.
;;
;; It's convenient for humans to represent these cards as suit/rank pairs, such
;; as H5 or DQ: the heart five and diamond queen respectively. But these forms
;; are not convenient for programmers, so to write a card game you need some way
;; to parse an input string into meaningful components. For purposes of
;; determining rank, we will define the cards to be valued from 0 (the two) to
;; 12 (the ace)
;;
;; Write a function which converts (for example) the string "SJ" into a map of
;; {:suit :spade, :rank 9}. A ten will always be represented with the single
;; character "T", rather than the two characters "10".
(fn card [code]
  (let [suits {\C :club, \D :diamond, \H :heart, \S :spade}
        ranks {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5,\8 6,
               \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}]
    {:suit (get suits (first code)) :rank (get ranks (second code))}))


;; http://www.4clojure.com/problem/100
;; Least Common Multiple
;; Write a function which calculates the least common multiple. Your function
;; should accept a variable number of positive integers or ratios.
(fn l-c-m [x y & zs]
  (first
   (filter (fn [a]
             (every? zero? (map (fn [b] (rem a b))
                                (cons y (into [] zs)))))
           (lazy-seq (map (partial * x) (rest (range)))))))


;; http://www.4clojure.com/problem/74
;; Filter Perfect Squares
;; Given a string of comma separated integers, write a function which returns a
;; new comma separated string that only contains the numbers which are perfect
;; squares.
(fn perfect-squares [s]
  (->> (clojure.string/split s #",")
       (map read-string)
       (filter #(zero?
                 (compare (Math/sqrt %)
                          (int (Math/sqrt %)))))
       (map str)
       (clojure.string/join ",")))


;; http://www.4clojure.com/problem/121
;; Universal Computation Engine
;; Given a mathematical formula in prefix notation, return a function that
;; calculates the value of the formula. The formula can contain nested
;; calculations using the four basic mathematical operators, numeric constants,
;; and symbols representing variables. The returned function has to accept a
;; single parameter containing the map of variable names to their values.
(fn uce' [fmla]
  (let [ops
        {'/ / '* * '+ + '- -}

        uce-fn
        (fn engine [expression substitutes]
          (let [operator (ops (first expression))]
            (apply operator (reduce (fn [acc v]
                                      (conj acc
                                            (or (substitutes v)
                                                (if (seq? v)
                                                  (engine v substitutes)
                                                  v))))
                                    [] (rest expression)))))]
    (fn [subs']
      (uce-fn fmla subs'))))

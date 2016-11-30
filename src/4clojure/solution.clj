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
(fn [coll n]
  (last (take (inc n) coll)))


;; http://www.4clojure.com/problem/22
;; Count a Sequence
;; Write a function which returns the total number of elements in a sequence.
#(.size (seq %))


;; http://www.4clojure.com/problem/23
;; Reverse a Sequence
;; Write a function which reverses a sequence.
(fn reverse' [coll]
  (reduce (fn [accum item]
            (cons item accum))
          []
          coll))


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
(fn max' [& args]
  (last (sort args)))


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
(fn dup-seq [coll]
  (interleave coll coll))


;; http://www.4clojure.com/problem/34
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
(fn interpose' [interposer coll]
  (reduce (fn [accum item]
            (conj accum interposer item))
          [(first coll)]
          (rest coll)))


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


;; http://www.4clojure.com/problem/77
;; Anagram Finder
;; Write a function which finds all the anagrams in a vector of words. A word x
;; is an anagram of word y if all the letters in x can be rearranged in a
;; different order to form y. Your function should return a set of sets, where
;; each sub-set is a group of words which are anagrams of each other. Each
;; sub-set should have at least two words. Words without any anagrams should not
;; be included in the result.
(fn anagram-finder' [words]
  (->> words
       (group-by sort)
       (map (comp set last))
       (filter (comp (partial < 1) count))
       set))

;; http://www.4clojure.com/problem/80
;; Perfect Numbers
;;A number is "perfect" if the sum of its divisors equal the number itself. 6 is
;; a perfect number because 1+2+3=6. Write a function which returns true for
;; perfect numbers and false otherwise.
(fn perfect-numbers' [x]
  (->> x
       (range 1)
       (filter (comp integer? (partial / x)))
       (reduce +)
       (= x)))


;; http://www.4clojure.com/problem/60
;; Sequence Reductions
;; Write a function which behaves like reduce, but returns each intermediate
;; value of the reduction. Your function must accept either two or three
;; arguments, and the return sequence must be lazy.
(fn reductions'
  ([f coll] (reductions' f (f (first coll)) (rest coll)))
  ([f v [h & t]]
   (lazy-seq (cons v (if (seq t)
                       (reductions' f (f v h) t)
                       [(f v h)])))))


;; http://www.4clojure.com/problem/69
;; Merge with a Function
;; Write a function which takes a function f and a variable number of maps. Your
;; function should return a map that consists of the rest of the maps conj-ed
;; onto the first. If a key occurs in more than one map, the mapping(s) from the
;; latter (left-to-right) should be combined with the mapping in the result by
;; calling (f val-in-result val-in-latter)
(fn mw-2 [f & maps]
  (let [r (fn [i a]
            (reduce-kv
             (fn [m k v]
               (let [existing (get m k)]
                 (assoc m k (if existing (f existing v) v))))
             i a))]
    (reduce (fn [v item] (r v item)) (first maps) (rest maps))))


;; http://www.4clojure.com/problem/147
;; Pascal's Trapezoid
;; Write a function that, for any given input vector of numbers, returns an
;; infinite lazy sequence of vectors, where each next one is constructed from
;; the previous following the rules used in Pascal's Triangle. For example, for
;; [3 1 2], the next row is [3 4 3 2].
;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you
;; use an arithmetic operator like + and the result is too large to fit into a
;; 64-bit integer, an exception is thrown. You can use +' to indicate that you
;; would rather overflow into Clojure's slower, arbitrary-precision bigint.
(fn p' [input]
  (lazy-seq
   (cons input (p' (mapv #(apply +' %)
                         (cons (list (first input))
                               (partition-all 2 1 input)))))))


;; http://www.4clojure.com/problem/96
;; Beauty is Symmetry
;; Let us define a binary tree as "symmetric" if the left half of the tree is
;; the mirror image of the right half of the tree. Write a predicate to
;; determine whether or not a given binary tree is symmetric. (see To Tree, or
;; not to Tree for a reminder on the tree representation we're using).
(fn sym [[root left right]]
  (letfn [(mirror? [l r]
            (or (every? nil? [l r])
                (and (= (first l) (first r))
                     (mirror? (second l) (last r))
                     (mirror? (last l) (second r)))))]
    (mirror? left right)))


;; http://www.4clojure.com/problem/102
;; intoCamelCase
;; When working with java, you often need to create an object with
;; fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this
;; until it's time to convert. Write a function which takes lower-case
;; hyphen-separated strings and converts them to camel-case strings.
(fn camel-case [word]
  (let [broken (clojure.string/split word #"-")
        start (first broken)
        others (rest broken)]
    (str start
         (clojure.string/join "" (map clojure.string/capitalize others)))))


;; http://www.4clojure.com/problem/146
;; Trees into tables
;; Because Clojure's for macro allows you to "walk" over multiple sequences in a
;; nested fashion, it is excellent for transforming all sorts of sequences. If
;; you don't want a sequence as your final output (say you want a map), you are
;; often still best-off using for, because you can produce a sequence and feed
;; it into a map, for example.

;; For this problem, your goal is to "flatten" a map of hashmaps. Each key in
;; your output map should be the "path" that you would have to take in the
;; original map to get to a value, so for example {1 {2 3}} should result in
;; {[1 2] 3}. You only need to flatten one level of maps: if one of the values
;; is a map, just leave it alone.
(fn tree-to-table [m]
  (apply merge (for [i m]
                 (reduce (fn [acc item]
                           (assoc acc [(first i) (first item)] (second item)))
                         {}
                         (second i)))))


;; http://www.4clojure.com/problem/153
;; Pairwise Disjoint Sets
;; Given a set of sets, create a function which returns true if no two of those
;; sets have any elements in common and false otherwise. Some of the test cases
;; are a bit tricky, so pay a little more attention to them.
(fn pairwise-disjoint?' [ss]
  (letfn [(all-pairs' [coll]
            (when-let [s (next coll)]
              (lazy-cat (for [y s] [(first coll) y])
                        (all-pairs' s))))]
    (->> (all-pairs' ss)
         (map (partial apply clojure.set/intersection))
         (not-any? seq))))


;; http://www.4clojure.com/problem/73
;; Analyze a Tic-Tac-Toe Board
;; A tic-tac-toe board is represented by a two dimensional vector. X is
;; represented by :x, O is represented by :o, and empty is represented by :e. A
;; player wins by placing three Xs or three Os in a horizontal, vertical, or
;; diagonal row. Write a function which analyzes a tic-tac-toe board and returns
;; :x if X has won, :o if O has won, and nil if neither player has won.
(fn ttt-winner [board]
  (let [filter-fn (fn [rows] (first (filter #(and (= 1 (count %))
                                                  (not= (first %) :e))
                                            (map distinct rows))))
        h (filter-fn board)
        v (filter-fn (partition 3 (for [x (range 3)
                                        row board]
                                    (nth row x))))
        d (filter-fn [[(nth (nth board 0) 0)
                       (nth (nth board 1) 1)
                       (nth (nth board 2) 2)]
                      [(nth (nth board 0) 2)
                       (nth (nth board 1) 1)
                       (nth (nth board 2) 0)]])]
    (first (or h v d))))

;; http://www.4clojure.com/problem/75
;; Euler's Totient Function
;; Two numbers are coprime if their greatest common divisor equals 1. Euler's
;; totient function f(x) is defined as the number of positive integers less than
;; x which are coprime to x. The special case f(1) equals 1. Write a function
;; which calculates Euler's totient function.
(fn totient [n]
  (letfn [(gcd [x y] (if (zero? y)
                       x
                       (recur y (rem x y))))]
    (count (if (= n 1)
             '(1)
             (filter
              (comp (partial = 1) (partial gcd n))
              (range 1 n))))))


;; http://www.4clojure.com/problem/86
;; Happy numbers
;; Happy numbers are positive integers that follow a particular formula: take
;; each individual digit, square it, and then sum the squares to get a new
;; number. Repeat with the new number and eventually, you might get to a number
;; whose squared sum is 1. This is a happy number. An unhappy number (or sad
;; number) is one that loops endlessly. Write a function that determines if a
;; number is happy or not.
(fn is-happy' [n]
  (letfn [(square [x] (* x x))]
    (cond (= 1 n) true
          (= 4 n) false
          :else (recur (->> n
                            str
                            seq
                            (map #(-> %
                                      str
                                      (Integer/parseInt)
                                      square))
                            (apply +))))))


;; http://www.4clojure.com/problem/78
;; Reimplement Trampoline
;; Reimplement the function described in "Intro to Trampoline".
(fn trampoline' [f & args]
  (let [outcome (apply f args)]
    (if (fn? outcome)
      (trampoline' outcome)
      outcome)))


;; http://www.4clojure.com/problem/115
;; The Balance of N
;; A balanced number is one whose component digits have the same sum on the left
;; and right halves of the number. Write a function which accepts an integer n,
;; and returns true if n is balanced.
(fn balance-of-n [n]
  (let [s (map (fn [x]
                 (->> x
                      str
                      Integer/parseInt))
               (str n))
        half (quot (count s) 2)]
    (= (reduce + (take half s))
       (reduce + (take-last half s)))))


;; http://www.4clojure.com/problem/85
;; Power Set
;; Write a function which generates the power set of a given set. The power set
;; of a set x is the set of all subsets of x, including the empty set and x
;; itself.
(fn power-set [superset]
  (reduce (fn [base-set item]
            (into base-set (map
                            (fn [s] (conj s item))
                            base-set)))
          #{#{}}
          superset))


;; http://www.4clojure.com/problem/98
;; Equivalence Classes
;; A function f defined on a domain D induces an equivalence relation on D, as
;; follows: a is equivalent to b with respect to f if and only if (f a) is equal
;; to (f b). Write a function with arguments f and D that computes the
;; equivalence classes of D with respect to f.
(fn equiv-classes [func domain]
  (set
   (map set
        (vals (reduce (fn [accum item]
                        (merge-with concat
                                    accum
                                    {(func item) [item]}))
                      {}
                      domain)))))


;; http://www.4clojure.com/problem/105
;; Identify keys and values
;; Given an input sequence of keywords and numbers, create a map such that each
;; key in the map is a keyword, and the value is a sequence of all the numbers
;; (if any) between it and the next keyword in the sequence.
(fn idkv [coll]
  (or (->> coll
           (partition-by keyword?)
           (partition 2)
           (map #(assoc
                  (into {} (for [x (butlast (first %))] [x []]))
                  (last (first %)) (second %)))
           (apply merge))
      {}))


;; http://www.4clojure.com/problem/137
;; Digits and bases
;; Write a function which returns a sequence of digits of a non-negative number
;; (first argument) in numerical system with an arbitrary base (second
;; argument). Digits should be represented with their integer values, e.g. 15
;; would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.
(fn digits-and-bases'
  ([num base] (digits-and-bases' num base []))
  ([num base accum]
   (let [result (int (/ num base))
         remainder (rem num base)]
     (if (< result base)
       (if (every? zero? [result remainder])
         (cons result accum)
         (concat [result remainder] accum))
       (recur result base (cons remainder accum))))))


;; http://www.4clojure.com/problem/144
;; Oscilrate
;; Write an oscillating iterate: a function that takes an initial value and a
;; variable number of functions. It should return a lazy sequence of the
;; functions applied to the value in order, restarting from the first function
;; after it hits the end.
(fn oscilrate [num & funcs]
  (reductions (fn [n f] (f n)) num (cycle funcs)))


;; http://www.4clojure.com/problem/110
;; Sequence of pronunciations
;; Write a function that returns a lazy sequence of "pronunciations" of a
;; sequence of numbers. A pronunciation of each element in the sequence consists
;; of the number of repeating identical numbers and the number itself. For
;; example, [1 1] is pronounced as [2 1] ("two ones"), which in turn is
;; pronounced as [1 2 1 1] ("one two, one one").
;; Your function should accept an initial sequence of numbers, and return an
;; infinite lazy sequence of pronunciations, each element being a pronunciation
;; of the previous element.
(fn seq-pron [coll]
  (let [pronouncer (fn [c] (reduce
                            (fn [a c]
                              (conj a (count c) (first c)))
                            []
                            (partition-by identity c)))
        seed (pronouncer coll)]
    (iterate pronouncer seed)))


;; http://www.4clojure.com/problem/158
;; Write a function that accepts a curried function of unknown arity n. Return
;; an equivalent function of n arguments.
(fn decurry [fns]
  (fn [& args]
    (reduce (fn [accum item] (accum item))
            (fns (first args)) (rest args))))


;; http://www.4clojure.com/problem/108
;; Lazy Searching
;; Given any number of sequences, each sorted from smallest to largest, find the
;; smallest single number which appears in all of the sequences. The sequences
;; may be infinite, so be careful to search lazily.
(fn lazy-search [& colls]
  (let [all-firsts (map first colls)
        min-first (apply min all-firsts)]
    (if (apply = all-firsts)
      min-first
      (recur (map (fn [coll] (drop-while #(= min-first %) coll)) colls)))))


;; http://www.4clojure.com/problem/93
;; Partially Flatten a Sequence
;; Write a function which flattens any nested combination of sequential things
;; (lists, vectors, etc.), but maintains the lowest level sequential items. The
;; result should be a sequence of sequences with only one level of nesting.
(fn pflatten [s]
  (let [c (atom [])
        accum (fn coll-fn [xs]
                (if-not (coll? (first xs))
                  (swap! c conj xs)
                  (do
                    (coll-fn (first xs))
                    (when (seq (rest xs)) (coll-fn (rest xs))))))]
    (accum s)
    @c))


;; http://www.4clojure.com/problem/114
;; Global take-while
;; take-while is great for filtering sequences, but it limited: you can only
;; examine a single item of the sequence at a time. What if you need to keep
;; track of some state as you go over the sequence?

;; Write a function which accepts an integer n, a predicate p, and a sequence.
;; It should return a lazy sequence of items in the list up to, but not
;; including, the nth item that satisfies the predicate.
(fn g-take-while [n pred xs]
  (loop [y n
         res []
         cs xs]
    (let [z (if (pred (first cs))
              (dec y)
              y)]
      (if (zero? z)
        res
        (recur z (conj res (first cs)) (rest cs))))))


;; http://www.4clojure.com/problem/92
;; Read Roman numerals
;; Roman numerals are easy to recognize, but not everyone knows all the rules
;; necessary to work with them. Write a function to parse a Roman-numeral string
;; and return the number it represents.

;; You can assume that the input will be well-formed, in upper-case, and follow
;; the subtractive principle. You don't need to handle any numbers greater than
;; MMMCMXCIX (3999), the largest number representable with ordinary letters.
(fn roman [s]
  (let [combos {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
        singles {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
    (loop [arabic 0
           roamin s]
      (if-not (seq roamin)
        arabic
        (let [n (get combos (clojure.string/join "" (take 2 roamin)))
              x (if n
                  (+ arabic n)
                  (+ arabic (get singles (str (first roamin)))))
              r (if n
                  (drop 2 roamin)
                  (rest roamin))]
          (recur x (clojure.string/join r)))))))


;; http://www.4clojure.com/problem/79
;; Triangle Minimal Path
;; Write a function which calculates the sum of the minimal path through a
;; triangle. The triangle is represented as a collection of vectors. The path
;; should start at the top of the triangle and move to an adjacent number on the
;; next row until the bottom of the triangle is reached.
(fn triangle-minimum-path' [xss]
  (let [rows (count xss)
        mpfn (fn mfn [i r accum]
               (if (zero? (- r rows))
                 accum
                 [(mfn i (inc r) (+ accum (nth (nth xss r) i)))
                  (mfn (inc i) (inc r) (+ accum (nth (nth xss r) (inc i))))]))]
    (->> (mpfn 0 1 (nth (nth xss 0) 0))
         flatten
         (apply min))))


;; http://www.4clojure.com/problem/132
;; Insert between two items
;; Write a function that takes a two-argument predicate, a value, and a
;; collection; and returns a new collection where the value is inserted between
;; every two items that satisfy the predicate.
(fn insert-between [pred v c]
  (lazy-seq (cond (empty? (rest c))
                  c
                  (pred (first c) (second c))
                  (cons (first c) (cons v (insert-between pred v (rest c))))
                  :else
                  (cons (first c) (insert-between pred v (rest c))))))


;; http://www.4clojure.com/problem/104
;; Write Roman Numerals
;; This is the inverse of Problem 92, but much easier. Given an integer smaller
;; than 4000, return the corresponding roman numeral in uppercase, adhering to
;; the subtractive principle.
(fn make-roman [arabic]
  (let [a->r {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "X" 90 "XC"
              100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}]
    (loop [r ""
           a arabic]
      (if-not (pos? a)
        r
        (let [biggest (apply max (map first (remove #(> (first %) a) a->r)))]
          (recur (str r (get a->r biggest)) (- a biggest)))))))


;; http://www.4clojure.com/problem/103
;; Generating k-combinations
;; Given a sequence S consisting of n elements generate all k-combinations of S,
;; i. e. generate all possible sets consisting of k distinct elements taken from
;; S. The number of k-combinations for a sequence is equal to the binomial
;; coefficient.
(fn kc [n arr]
  (letfn [(helper [acc res]
            (if (= n (count acc))
              acc
              (map (fn [item]
                     (helper (conj acc item) (remove #(= % item) res)))
                   res)))]
    (set (flatten (helper #{} arr)))))


;; http://www.4clojure.com/problem/116
;; Prime Sandwich
;; A balanced prime is a prime number which is also the mean of the primes
;; directly before and after it in the sequence of valid primes. Create a
;; function which takes an integer n, and returns true if it is a balanced
;; prime.
(fn prime-sandwich [c]
  (let [prime? (fn [n]
                 (if (even? n) false
                     (let [root (num (int (Math/sqrt n)))]
                       (loop [i 3]
                         (if (> i root) true
                             (if (zero? (mod n i)) false
                                 (recur (+ i 2))))))))
        prime-from (fn [c]
                     (first (drop-while #(not (prime? %)) c)))
        previous-prime (->> c
                            dec
                            (iterate dec)
                            prime-from)
        next-prime (->> c
                        inc
                        (iterate inc)
                        prime-from)]
    (and (> c 4) (prime? c) (-> previous-prime
                                (+ next-prime)
                                (/ 2)
                                (= c)))))


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


;; http://www.4clojure.com/problem/148
;; The Big Divide
;; Write a function which calculates the sum of all natural numbers under n
;; (first argument) which are evenly divisible by at least one of a and b
;; (second and third argument). Numbers a and b are guaranteed to be
;; coprimes.
;; Note: Some test cases have a very large n, so the most obvious solution
;; will exceed the time limit.
(fn big-divide [n a b]
  (letfn [(sum-mults [x]
            (let [y (quot (dec n) x)]
              (*' x
                  (/ (*' y
                         (inc y))
                     2))))]
    (- (+ (sum-mults a)
          (sum-mults b))
       (sum-mults (* a b)))))


;; http://www.4clojure.com/problem/171
;; Intervals
;; Write a function that takes a sequence of integers and returns a sequence of
;; "intervals". Each interval is a a vector of two integers, start and end, such
;; that all integers between start and end (inclusive) are contained in the
;; input sequence.
(fn intervals' [xs]
  (->> xs
       sort
       distinct
       (reduce (fn [acc item]
                 (if (or (empty? acc)
                         (> (Math/abs (- (first (first acc)) item)) 1))
                   (cons [item] acc)
                   (cons (cons item (first acc)) (rest acc)))) [])
       (map (fn [grp] [(apply min grp) (apply max grp)]))
       reverse))


;; http://www.4clojure.com/problem/177
;; Balancing Brackets
;; When parsing a snippet of code it's often a good idea to do a sanity check to
;; see if all the brackets match up. Write a function that takes in a string and
;; returns truthy if all square [ ] round ( ) and curly { } brackets are properly
;; paired and legally nested, or returns falsey otherwise.
(fn balanced? [s]
  (let [paired-pattern #"\(\)|\[\]|\{\}"]
    (loop [target (apply str (re-seq #"[\{|\}|\(|\)|\[|\]]" s))]
      (cond (empty? target) true
            (not (re-find paired-pattern target)) false
            :else (recur (clojure.string/replace target paired-pattern ""))))))


;; http://www.4clojure.com/problem/solutions/84
;; Transitive Closure
;; Write a function which generates the transitive closure of a binary relation.
;; The relation will be represented as a set of 2 item vectors.
(fn t-closure' [xss]
  (letfn [(match-fn [item all]
            (filter #(= (last item)
                        (first %))
                    (clojure.set/difference all #{item})))
          (pair-fn [target matches]
            (when (seq matches)
              (map (fn [i] [(first target) (last i)])
                   matches)))
          (reduce-fn [pairs]
            (reduce (fn [accum item]
                      (let [matches (match-fn item pairs)
                            paired (pair-fn item matches)]
                        (if (seq paired)
                          (apply conj accum paired)
                          accum))) [] pairs))
          (builder [pairs]
            (-> pairs
                reduce-fn
                (concat pairs)
                set))]
    (loop [yss xss]
      (let [new-pairs (builder yss)]
        (if (= (count yss) (count new-pairs))
          yss
          (recur new-pairs))))))

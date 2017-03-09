(ns scratch)


(defn pal-nums [x]
  (letfn [(pal-coll [n]
            (loop [y n
                   accum []]
              (if (< y 10)
                (cons y accum)
                (recur (long (/ y 10)) (cons (rem y 10) accum)))))
          (val-for-part [num-coll]
            (apply + (map-indexed
                      (fn [index item]
                        (long (* (java.lang.Math/pow 10 index) item)))
                      num-coll)))
          (palindrome [n]
            (let [data (pal-coll n)
                  num-len (count data)
                  midway (long (/ num-len 2))
                  first-half (take midway data)
                  last-half (drop midway data)
                  val-for-head (val-for-part (if (even? num-len)
                                               (reverse first-half)
                                               (reverse (take (inc midway) data))))
                  head (if (<= (val-for-part first-half)
                               (val-for-part (reverse last-half)))
                         (inc val-for-head)
                         val-for-head)
                  head-coll (pal-coll head)
                  current (val-for-part (concat head-coll (->> head-coll
                                                               (take midway)
                                                               reverse)))]
              (lazy-seq (cons current (palindrome current)))))]
    (if (= (pal-coll x) (reverse (pal-coll x)))
      (lazy-seq (cons x (pal-nums (inc x))))
      (palindrome x))))


(take 26 (pal-nums 0))

(take 16 (pal-nums 162))

(take 6 (pal-nums 1234550000))


(= (first (pal-nums (* 111111111 111111111)))
   (* 111111111 111111111))


(= (set (take 199 (pal-nums 0)))
   (set (map #(first (pal-nums %)) (range 0 10000))))

(= true
   (apply < (take 6666 (pal-nums 9999999))))


(= (nth (pal-nums 0) 10101)
   9102019)

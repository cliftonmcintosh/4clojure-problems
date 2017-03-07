(ns scratch)

(defn pal-numbers [x]
  (letfn [(palindrome [n]
            (let [num-str (str n)
                  num-len (count num-str)
                  midway (int (/ num-len 2))
                  first-half (->> num-str
                                  (take midway)
                                  reverse
                                  (apply str))
                  last-half (->> num-str
                                 (drop midway)
                                 (apply str))
                  head (if (<= (Long/valueOf first-half)
                               (Long/valueOf last-half))
                         (inc (Long/valueOf (apply str (take
                                                        (if (even? num-len)
                                                          midway
                                                          (inc midway)) num-str))))
                         (Long/valueOf (apply str (take (if (even? num-len)
                                                          midway
                                                          (inc midway))  num-str))))
                  current (Long/valueOf (str head (->> head
                                                       str
                                                       (take midway)
                                                       reverse
                                                       (apply str))))]
              (lazy-seq (cons current (palindrome current)))))]
    (if (= (str x) (clojure.string/reverse (str x)))
      (lazy-seq (cons x (pal-numbers (inc x))))
      (palindrome x))))

(take 26 (pal-numbers 0))

(take 16 (pal-numbers 162))

(take 6 (pal-numbers 1234550000))

(first (pal-numbers (* 111111111 111111111)))

(= (first (pal-numbers (* 111111111 111111111)))
   (* 111111111 111111111))

(apply < (take 6666 (pal-numbers 9999999)))

(= (nth (pal-numbers 0) 10101)
   9102019)

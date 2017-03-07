(ns scratch)

(defn next-palindrome [n]
  (if (> 10 n)
    (lazy-seq (cons (inc n) (next-palindrome (inc n))))
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
      (lazy-seq (cons current (next-palindrome current))))))

(take 2 (next-palindrome 0))

(take 16 (next-palindrome 162))

(take 6 (next-palindrome 1234550000))

(first (next-palindrome (* 111111111 111111111)))

(apply < (take 6666 (next-palindrome 9999999)))


;; def next_palindrome(n):
;; """
;; Given a non-negative integer n, return the first integer strictly
;; greater than n whose decimal representation is palindromic.
;;
;; """
;;   s = str(n + 1)
;;   l = len(s)
;;   if s[:l//2][::-1] < s[(l+1)//2:]:
;;     head = str(int(s[:(l+1)//2])+1)
;;   else:
;;     head = s[:(l+1)//2]
;;   return int(head + head[:l//2][::-1])

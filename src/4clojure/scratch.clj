(ns scratch)

(defn next-palindrome [n]
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
        head (if (< (Integer/valueOf first-half)
                    (Integer/valueOf last-half))
               (inc (Integer/valueOf (apply str (take
                                                 (if (even? num-len)
                                                   midway
                                                   (inc midway)) num-str))))
               (Integer/valueOf (apply str (take (if (even? num-len)
                                                   midway
                                                   (inc midway))  num-str))))]
    [head (->> head
               str
               (take midway)
               reverse
               (apply str))]))

(next-palindrome 162)

(next-palindrome 1234550000)


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

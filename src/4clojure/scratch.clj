(ns scratch)


(defn sequs-h [n xs]
  (letfn [(reduce-fn [accum item]
            (let [total (apply + (flatten accum))]
              (cond (sequential? item)
                    (conj accum (sequs-h (- n total) item))
                    (> (+ total item) n)
                    (reduced accum)
                    :else
                    (conj accum item))))]
    (reduce reduce-fn [] xs)))



;; works for all cases except the negative n
(defn sequs-h' [n xs]
  (println "n: " n)
  (println "xs: " xs)
  (letfn [(reduce-fn [accum item]
            (println "accum: " accum)
            (println "item: " item)
            (let [total (apply + (flatten accum))]
              (println "total: " total)
              (cond (and (number? item)
                         (> (+ total item) n))
                    accum
                    (sequential? item)
                    (conj accum (sequs-h' (- n total) item))
                    :else
                    (conj accum item))))
          (taker [current]
            (println "current: " current)
            (println "last current: " (last current))
            (>= n (apply + (flatten (last current)))))]
    #_(last (take-while #(>= n (apply + (flatten (last %))))
                        (reductions reduce-fn [] xs)))
    (last (take-while taker
                      (reductions reduce-fn [] xs)))))



(def __ sequs-h')

(=  (__ 10 [1 2 [3 [4 5] 6] 7])
    '(1 2 (3 (4))))

(=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
    '(1 2 (3 (4 (5 (6 (7)))))))

(=  (__ 9 (range 20))
    '(0 1 2 3))

(=  (__ 1 [[[[[1]]]]])
    '(((((1))))))


(=  (__ 0 [1 2 [3 [4 5] 6] 7])
    '())

(=  (__ 0 [0 0 [0 [0]]])
    '(0 0 (0 (0))))

(=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
    '(-10 (1 (2 3 (4)))))

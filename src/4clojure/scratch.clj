(ns scratch)

(defn big-divide [n a b]
  (apply +
         (filter #(or (zero? (rem % a)) (zero? (rem % b)))
                 (range n))))

(defn big-divide' [n a b]
  (letfn [(mplier [constant]
            (loop [accum []
                   multiplier 1]
              (let [mult (* constant multiplier)]
                (if-not (> n mult)
                  (set accum)
                  (recur (conj accum mult) (inc multiplier))))))]
    (apply + (clojure.set/union (mplier a) (mplier b)))))

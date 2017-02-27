(ns scratch)


(def example ["      "
              " ##   "
              " ##   "
              "   ## "
              "   ## "
              "      "])


(defn game-of-life [rs]
  (let [height (count rs)
        indexed-chars
        (apply merge
               (for [r (range height)]
                 (->> r
                      (nth rs)
                      (map-indexed (fn [idx itm] [[r idx] itm]))
                      (reduce
                       (fn [accum itm]
                         (merge accum {(first itm) (second itm)}))
                       {}))))]
    (filter #(= \# (val %)) indexed-chars)))


(game-of-life example)

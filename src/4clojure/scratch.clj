(ns scratch)


(def example ["      "
              " ##   "
              " ##   "
              "   ## "
              "   ## "
              "      "])


(defn game-of-life [rs]
  (let [row-count (count rs)
        column-count (count (first rs))
        indexed-chars-map
        (apply merge (for [r (range row-count)
                           c (range column-count)]
                       {[r c] (get-in example [r c])}))
        neighbors-fn (fn [r c]
                       (let [previous-row (dec r)
                             next-row (inc r)
                             previous-column (dec c)
                             next-column (inc c)]
                         [(get indexed-chars-map [previous-row c])
                          (get indexed-chars-map [previous-row next-column])
                          (get indexed-chars-map [previous-row previous-column])
                          (get indexed-chars-map [previous-row previous-column])
                          (get indexed-chars-map [previous-row previous-column])
                          (get indexed-chars-map [previous-row previous-column])
                          (get indexed-chars-map [previous-row previous-column])
                          (get indexed-chars-map [previous-row previous-column])]))]
    indexed-chars-map))


(game-of-life example)


;;; get-in works for vectors


(map #(apply str %) (partition 6 (for [r (range 6)
                                       c (range 6)]
                                   (get-in example [r c]))))


(for [r (range 6)
      c (range 6)]
  (let [chrctr (get-in example [r c])]
    {[r c] chrctr}))

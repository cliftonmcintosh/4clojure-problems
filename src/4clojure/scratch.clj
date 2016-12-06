(ns scratch)


(defn word-chains [ws]
  (let [freqs (map frequencies ws)
        finder (fn [x y]
                 (as-> (merge-with - x y) $
                   (group-by val $)
                   (count (get $ 1))
                   (or (= 1 $) (= 2 $))))]
    (every? #(contains? % true)
            (map set (for [f freqs]
                       (map #(finder f %) (remove #(= f %) freqs)))))))

(word-chains #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})

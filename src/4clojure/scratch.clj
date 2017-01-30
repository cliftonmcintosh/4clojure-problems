(ns scratch)


(defn tricky [cards trump]
  (let [lead (:suit (first cards))
        find-best-suit (fn [suited]
                         (if (and trump (seq (trump suited)))
                           (trump suited)
                           (lead suited)))]
    (->> cards
         (group-by :suit)
         find-best-suit
         (sort-by :rank)
         last)))

(def cards [{:suit :spade :rank 2}
            {:suit :spade :rank 3}
            {:suit :club :rank 10}])

(tricky cards :heart)


(= {:suit :club :rank 10} ((tricky :club) [{:suit :spade :rank 2}
                                       {:suit :club :rank 10}]))

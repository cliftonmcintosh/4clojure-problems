(ns scratch)


(defn uce' [fmla]
  (let [ops
        {'/ / '* * '+ + '- -}

        uce-fn
        (fn engine [expression substitutes]
          (let [operator (ops (first expression))]
            (apply operator (reduce (fn [acc v]
                                      (conj acc
                                            (or (substitutes v)
                                                (if (seq? v)
                                                  (engine v substitutes)
                                                  v))))
                                    [] (rest expression)))))]
    (fn [subs']
      (uce-fn fmla subs'))))

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


(defn balanced? [s]
  (letfn [(balanced-markers? [opener closer match-expr to-evaluate]
            (loop [target to-evaluate]
              (println "target: " target)
              (cond (not (or (clojure.string/includes? target opener)
                             (clojure.string/includes? target closer)))
                    true

                    (nil? (re-find match-expr target))
                    false

                    :else
                    (recur (clojure.string/replace-first target match-expr "")))))]
    (every? #(= true %)
            (for [matchers [{:opener "(" :closer ")" :matcher #"\(\)"}
                            {:opener "[" :closer "]" :matcher #"\[\]"}
                            {:opener "{" :closer "}" :matcher #"\{\}"}]]
              (balanced-markers?
                (:opener matchers)
                (:closer matchers)
                (:matcher matchers)
                (apply str (re-seq #"[\{|\}|\(|\)|\[|\]]" s)))))))

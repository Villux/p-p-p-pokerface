(ns p-p-p-pokerface)

(def rank-replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})


(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (not (empty? (filter #(< 1 %) (vals (frequencies (map rank hand)))))))


(defn three-of-a-kind? [hand]
  (not (empty? (filter #(< 2 %) (vals (frequencies (map rank hand)))))))


(defn four-of-a-kind? [hand]
  (not (empty? (filter #(< 3 %) (vals (frequencies (map rank hand)))))))


(defn flush? [hand]
  (not (empty? (filter #(< 4 %) (vals (frequencies (map suit hand)))))))


(defn full-house? [hand]
  (if (= (sort (vals (frequencies (map rank hand)))) [2 3])
    true
    false ))


(defn two-pairs? [hand]
  (if (= (reduce + (filter #(= (mod % 2) 0) (vals (frequencies (map rank hand))))) 4)
    true
    false))

(defn straight? [hand]
  (let [ high-ace-hand  (sort (map rank hand))
         low-ace-hand (sort (replace {14 1} high-ace-hand))
         min-value-hand-high (apply min high-ace-hand)
         min-value-hand-low (apply min low-ace-hand)]
    (if (or (= high-ace-hand (range min-value-hand-high (+ min-value-hand-high 5)))
            (= low-ace-hand (range min-value-hand-low (+ min-value-hand-low 5))))
      true
      false )))


(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false ))


(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0 ))




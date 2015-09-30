(ns p-p-p-pokerface)

;; Testing material

(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

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

(pair? pair-hand)  ;=> true
(pair? high-seven) ;=> false

(defn three-of-a-kind? [hand]
  (not (empty? (filter #(< 2 %) (vals (frequencies (map rank hand)))))))

(three-of-a-kind? two-pairs-hand)       ;=> false
(three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand]
  (not (empty? (filter #(< 2 %) (vals (frequencies (map rank hand)))))))

(four-of-a-kind? two-pairs-hand)      ;=> false
(four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn flush? [hand]
  (not (empty? (filter #(< 4 %) (vals (frequencies (map suit hand)))))))

(flush? pair-hand)  ;=> false
(flush? flush-hand) ;=> true

(defn full-house? [hand]
  (if (= (count (vals (frequencies (map rank hand)))) 2)
    true
    false ))

(full-house? three-of-a-kind-hand) ;=> false
(full-house? full-house-hand)      ;=> true

(reduce + '(1 2))

(reduce + (filter #(= (mod % 2) 0)  (vals (frequencies (map rank four-of-a-kind-hand)))))
(filter #(= (mod % 2) 0)  (vals (frequencies (map rank two-pairs-hand))))

(defn two-pairs? [hand]
  (if (= (reduce + (filter #(= (mod % 2) 0) (vals (frequencies (map rank hand))))) 4)
    true
    false))

(two-pairs? two-pairs-hand)      ;=> true
(two-pairs? pair-hand)           ;=> false
(two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)

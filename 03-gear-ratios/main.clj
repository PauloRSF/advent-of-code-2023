(ns user (:use clojure.pprint))
(ns user (:use clojure.math))
(ns user (:use clojure.set))

;; This looks and performs like absolute crap. I'll improve it later

(let [zero (int \0)
      nine (int \9)]
  (defn is-digit? [c] (<= zero (int c) nine)))

(def is-right-adjacent (fn [lhs rhs] (= (lhs :x) (+ 1 (rhs :x)))))

(def get-3x3-neighborhood (fn [x y]
                            (set
                             [(seq [(- x 1) (- y 1)])
                              (seq [x (- y 1)])
                              (seq [(+ x 1) (- y 1)])
                              (seq [(- x 1) y])
                              (seq [x y])
                              (seq [(+ x 1) y])
                              (seq [(- x 1) (+ y 1)])
                              (seq [x (+ y 1)])
                              (seq [(+ x 1) (+ y 1)])])))

(def enumerate-lines-chars (fn [x] (map (fn [z] (map-indexed #({:y %1 :ch %2}) (filter #(not= %1 \.) z))) (seq x))))

(def add-line-index-to-elements (fn [x] (map-indexed (fn [i z] (map (fn [m] (assoc m :x i)) z)) (seq x))))

(def flatten-elements (partial apply concat))

(def is-gear-entry #(= (%1 :ch) \*))

(def is-symbol-entry #(and (not= (%1 :ch) \.) (not (is-digit? (%1 :ch)))))

(def is-digit-entry #(is-digit? (%1 :ch)))

(def pick-entry-coords #(select-keys %1 [:x :y]))

(def get-entries-coords (comp
                         #(set %)
                         #(map pick-entry-coords %)
                         #(filter is-gear-entry %)))

(def process-input (comp
                    flatten-elements ;; ((0 0 A), (0 1 B), (1 0 C), (1 1 D))
                    add-line-index-to-elements ;; (((0 0 A), (0 1 B)), ((1 0 C), (1 1 D)))
                    enumerate-lines-chars ;; (((0 A), (1 B)), ((0 C), (1 D)))
                    clojure.string/split-lines)) ;; [AB CD]

(defn main []
  (def input (slurp "input.txt"))

  (def enumerated-chars (process-input input))

  (def symbol-entries (filter is-symbol-entry enumerated-chars))

  (def digit-entries (filter is-digit-entry enumerated-chars))

  (def asd (reduce
            (fn [acc k]
              (if (is-right-adjacent k (first (first acc)))
                (conj (drop 1 acc) (conj (first acc) k))
                (conj acc (seq [k]))))
            (seq [(seq [(first digit-entries)])])
            (drop 1 digit-entries)))

  (def qwe (map (fn [x]
                  (reduce
                   (fn [acc k]
                     (seq [(+ (first acc) (* (int (pow 10 (first k))) (Character/digit (nth (second k) 2) 10))) (union (get-3x3-neighborhood (first (second k)) (second (second k))) (second acc))]))
                   (seq [0 (set [])])
                   (map-indexed vector x)))
                asd))

  (def symbol-coords (get-entries-coords symbol-entries))

  (def ant (filter (fn [k] (not (empty? (intersection symbol-coords (second k))))) qwe))

  (def sum-of-part-numbers (reduce (fn [acc k] (+ acc (first k))) 0 ant))

  (println "Sum of all part numbers:" sum-of-part-numbers)

  (def gear-coords (get-entries-coords (filter is-gear-entry symbol-entries)))

  (def abn (filter (fn [m] (= 2 (count m))) (map (fn [k] (filter (fn [x] (contains? (second x) k)) qwe)) gear-coords)))

  (def sum-of-gear-ratios (reduce (fn [acc k] (+ acc (* (first (first k)) (first (second k))))) 0 abn))

  (println "Sum of all gear ratios:" sum-of-gear-ratios))

(main)

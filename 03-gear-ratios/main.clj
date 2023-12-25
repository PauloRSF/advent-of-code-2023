(ns user (:use clojure.pprint))
(ns user (:use clojure.math))
(ns user (:use clojure.set))

;; This looks and performs like absolute crap. I'll improve it later

(let [zero (int \0)
      nine (int \9)]
  (defn is-digit? [c] (<= zero (int c) nine)))

(def is-right-adjacent (fn [lhs rhs] (= (second lhs) (+ 1 (second rhs)))))

(def enumerate-string-chars
  (fn [str]
    (def enumerated-row-and-columns (map-indexed
                                     vector
                                     (map
                                      (fn [l] (map-indexed vector (char-array l)))
                                      (clojure.string/split-lines input))))

    (apply concat (map
                   (fn [k]
                     (map
                      (fn [x] (seq [(first k) (first x) (second x)]))
                      (second k)))
                   enumerated-row-and-columns))))

(def get-adjacents (fn [x y]
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

(defn main []
  (def input (slurp "input.txt"))

  (def enumerated-chars (enumerate-string-chars input))

  (def symbols (filter (fn [x] (def k (nth x 2)) (and (not= k \.) (not (is-digit? k)))) enumerated-chars))

  (def digits (filter (fn [x] (def k (nth x 2)) (is-digit? k)) enumerated-chars))

  (def asd (reduce
            (fn [acc k]
              (if (is-right-adjacent k (first (first acc)))
                (conj (drop 1 acc) (conj (first acc) k))
                (conj acc (seq [k]))))
            (seq [(seq [(first digits)])])
            (drop 1 digits)))

  (def qwe (map (fn [x]
                  (reduce
                   (fn [acc k]
                     (seq [(+ (first acc) (* (int (pow 10 (first k))) (Character/digit (nth (second k) 2) 10))) (union (get-adjacents (first (second k)) (second (second k))) (second acc))]))
                   (seq [0 (set [])])
                   (map-indexed vector x)))
                asd))

  (def dit (set (map (fn [x] (seq [(first x) (second x)])) symbols)))

  (def ant (filter (fn [k] (not (empty? (intersection dit (second k))))) qwe))

  (def nic (set (map (fn [x] (seq [(first x) (second x)])) (filter (fn [k] (= (nth k 2) \*)) symbols))))

  (def abn (filter (fn [m] (= 2 (count m))) (map (fn [k] (filter (fn [x] (contains? (second x) k)) qwe)) nic)))

  (def sum-of-part-numbers (reduce (fn [acc k] (+ acc (first k))) 0 ant))
  (def sum-of-gear-ratios (reduce (fn [acc k] (+ acc (* (first (first k)) (first (second k))))) 0 abn))

  (println "Sum of all part numbers:" sum-of-part-numbers)
  (println "Sum of all gear ratios:" sum-of-gear-ratios))

(main)

(ns user
  (:use [clojure.math :only [pow]])
  (:use [clojure.set :only [intersection union]])
  (:use [clojure.string :only [split-lines]]))

(let [zero (int \0)
      nine (int \9)]
  (defn is-digit? [c] (<= zero (int c) nine)))

(def is-right-adjacent (fn [lhs rhs] (= (lhs :x) (+ 1 (rhs :x)))))

(def get-entry-3x3-neighborhood (fn [entry]
                                  (let [x (entry :x) y (entry :y)] (set
                                                                    [(seq [(- x 1) (- y 1)])
                                                                     (seq [x (- y 1)])
                                                                     (seq [(+ x 1) (- y 1)])
                                                                     (seq [(- x 1) y])
                                                                     (seq [x y])
                                                                     (seq [(+ x 1) y])
                                                                     (seq [(- x 1) (+ y 1)])
                                                                     (seq [x (+ y 1)])
                                                                     (seq [(+ x 1) (+ y 1)])]))))

(def enumerate-lines-chars (fn [x] (map (fn [z] (map-indexed (fn [i k] {:x i :ch k}) z)) (seq x))))

(def add-line-index-to-elements (fn [x] (map-indexed (fn [i z] (map (fn [m] (assoc m :y i)) z)) (seq x))))

(def flatten-elements (partial apply concat))

(def is-asterisk-entry #(= (%1 :ch) \*))

(def is-dot-entry #(= (%1 :ch) \.))

(def is-digit-entry #(is-digit? (%1 :ch)))

(def is-symbol-entry #(and (not (is-dot-entry %1)) (not (is-digit-entry %1))))

(defn remove-dot-entries [entries] (filter #(not (is-dot-entry %1)) entries))

;; ------------------
;; Input parser
;; ------------------

(def parse-input-into-entries (comp
                               remove-dot-entries
                               flatten-elements ;; ((0 0 A), (0 1 B), (1 0 C), (1 1 D))
                               add-line-index-to-elements ;; (((0 0 A), (0 1 B)), ((1 0 C), (1 1 D)))
                               enumerate-lines-chars ;; (((0 A), (1 B)), ((0 C), (1 D)))
                               split-lines)) ;; [AB CD]

(defn read-input-file [] (slurp "input.txt"))

(defn group-entries [entries] {:symbols (filter is-symbol-entry entries) :digits (filter is-digit-entry entries)})

(defn convert-entry-group-to-part-number-with-neighborhood
  [entry-group]
  (reduce
   (fn [acc entry]
     {:number (+ (acc :number) (entry :value))
      :neighborhood (union (get-entry-3x3-neighborhood entry) (acc :neighborhood))})
   {:number 0 :neighborhood (set [])}
   entry-group))

(defn add-value-with-magnitude-to-digit-entry-group
  [entry-group]
  (map-indexed
   #(assoc %2 :value (* (int (pow 10 %1)) (Character/digit (%2 :ch) 10)))
   entry-group))

(defn group-adjacent-digits-entries
  [digit-entries]
  (reduce
   (fn [acc entry]
     (if (is-right-adjacent entry (first (first acc)))
       (conj (drop 1 acc) (conj (first acc) entry))
       (conj acc (seq [entry]))))
   (seq [(seq [(first digit-entries)])])
   (drop 1 digit-entries)))

(def convert-digit-entries-to-part-numbers (comp
                                            #(map convert-entry-group-to-part-number-with-neighborhood %1)
                                            #(map add-value-with-magnitude-to-digit-entry-group %1)
                                            group-adjacent-digits-entries))

(defn build-part-numbers-from-digit-entries [{digits :digits symbols :symbols}]
  {:symbols symbols :part-numbers (convert-digit-entries-to-part-numbers digits)})

(def parse-input (comp
                  build-part-numbers-from-digit-entries
                  group-entries
                  parse-input-into-entries
                  read-input-file))

;; ------------------
;; First solution
;; ------------------

(defn get-part-numbers-adjacent-to-symbols [part-numbers symbols]
  (let [symbol-coords (set (map #(seq [(%1 :x) (%1 :y)]) symbols))]
    (filter
     #(seq (intersection symbol-coords (%1 :neighborhood)))
     part-numbers)))

(defn compute-sum-of-part-numbers [part-numbers symbols]
  (reduce #(+ %1 (%2 :number)) 0 (get-part-numbers-adjacent-to-symbols part-numbers symbols)))

;; ------------------
;; Second solution
;; ------------------

(defn get-adjacent-part-numbers [entry part-numbers]
  (let [entry-coords (seq [(entry :x) (entry :y)])]
    (filter #(contains? (%1 :neighborhood) entry-coords) part-numbers)))

(defn get-gear-ratio [part-numbers] (* ((first part-numbers) :number) ((second part-numbers) :number)))

;; ------------------
;; Main
;; ------------------

(defn compute-sum-of-gear-ratios
  [part-numbers symbols]
  ((comp
    #(reduce + 0 %1)
    #(map get-gear-ratio %1)
    #(filter (fn [adjacent-part-numbers] (= 2 (count adjacent-part-numbers))) %1)
    #(map (fn [entry] (get-adjacent-part-numbers entry part-numbers)) %1)
    #(filter is-asterisk-entry symbols))))

(defn compute-solutions [{part-numbers :part-numbers symbols :symbols}]
  {:sum-of-part-numbers (compute-sum-of-part-numbers part-numbers symbols)
   :sum-of-gear-ratios (compute-sum-of-gear-ratios part-numbers symbols)})

(defn print-solutions [{sum-of-part-numbers :sum-of-part-numbers sum-of-gear-ratios :sum-of-gear-ratios}]
  (println "Sum of all part numbers:" sum-of-part-numbers)
  (println "Sum of all gear ratios:" sum-of-gear-ratios))

(def main (comp
           print-solutions
           compute-solutions
           parse-input))

(main)

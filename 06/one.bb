#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./one.bb
(def *lines* (line-seq (io/reader *in*)))

(defn parse-cell [c]
  (or
    ({ "+" +
       "*" * } c)
    (-> c parse-double int)))

(defn parse-line [ln]
  (let [xs (-> ln s/trim (s/split #"\s+"))]
    (into [] (map parse-cell xs))))

(defn exprs [lns]
  (for [x (range (count (first lns)))]
    (map #(nth % x) lns)))


(defn eval [expr]
  (apply (last expr) (butlast expr)))

(prn
  (->> *lines*
       (map parse-line)
       exprs
       (map eval)
       (reduce + 0)))

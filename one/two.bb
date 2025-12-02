#!/usr/bin/env bb -i


;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))


(defn rotate [value amt]
  (let [new-val (rem (+ value amt) 100)]
    (cond
      (neg? new-val) (+ new-val 100)
      (< 99 new-val) (- new-val 100)
      :else          new-val)))

(defn rotations [value amt]
  (let [raw-pos  (+ value amt)
        raw-rots (abs (quot raw-pos 100))]
    (if (and (> value 0) (<= raw-pos 0))
      (inc raw-rots)
      raw-rots)))


(defn rotate-noting-zeroes [[value zeroes] action]
  (let [new-val (rotate value action)
        new-zs  (+ zeroes (rotations value action))]
    [new-val new-zs]))


(defn parse-line [ln]
  (let [[_ dir dist] (re-find #"([LR]{1})(\d+)" ln)
        distance     (json/parse-string dist)]
    (if (= "L" dir) (* -1 distance) distance)))
    

(def result
  (->> *lines*
    (map parse-line)
    ;; (reductions) is useful debugging here
    (reduce rotate-noting-zeroes [50 0])
    second))


(prn result)


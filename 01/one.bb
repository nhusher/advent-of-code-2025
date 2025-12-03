#!/usr/bin/env bb -i


;; cat input.txt | ./one.bb
(def *lines* (line-seq (io/reader *in*)))


(defn rotate [value amt]
  ; handle values > 100 or < -100 using remainder fn
  (let [new-val (rem (+ value amt) 100)]
  ; if negative, correct to positive dial pos'n
    (if (neg? new-val) (+ new-val 100) new-val)))


(defn rotate-noting-zeroes [[value zeroes] action]
  (let [new-val (rotate value action)
        ; any time we land on zero, record that
        new-zs  (if (zero? new-val) (inc zeroes) zeroes)]
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


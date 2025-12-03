#!/usr/bin/env bb -i


;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))


(defn rotate [value amt]
  ; handle values > 100 or < -100 using remainder fn
  (let [new-val (rem (+ value amt) 100)]
  ; if negative, correct to positive dial pos'n
    (if (neg? new-val) (+ new-val 100) new-val)))


(defn rotations [value amt]
  ; given a dial position and turn, calculate how many times we go past or land
  ; on zero. logic here is fiddly.
  (let [posn (+ value amt)
        rots (abs (quot posn 100))]
    (if (and (> value 0) (<= posn 0))
      (inc rots)
      rots)))


(defn rotate-noting-zeroes [[value zeroes] action]
  (let [new-val (rotate value action)
        ; find how many times we passed zero and add it to the count
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


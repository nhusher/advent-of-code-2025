#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))


(defn parse-line [ln]
   ; mapping a string returns chars, which json/parse-string can't handle
  (map (comp json/parse-string str) ln))


(defn max-val [vs] (apply max vs))


; get the highest value from the set of values, except for the last n vals,
; then get the value from everything after the highest, and recurse.
(defn nth-highest-ordered-vals [number values]
  (loop [res []
         n   number
         vs  values]

    (if (zero? n) res
      (let [mv (max-val (drop-last (dec n) vs))]
        (recur
          (conj res mv)
          (dec n)
          (rest (drop-while #(< % mv) vs)))))))


; bigint here because i can't remember how big clojure ints are :X
(defn sum-joltage [vs]
  (reduce #(+ (* 10 %1) %2) 0N vs))


(prn
  (->> *lines*
      (map parse-line)
      (map #(nth-highest-ordered-vals 12 %))
      (map sum-joltage)
      (reduce + 0)))
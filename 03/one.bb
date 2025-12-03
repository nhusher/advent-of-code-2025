#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./one.bb
(def *lines* (line-seq (io/reader *in*)))

(defn parse-line [ln]
   ; mapping a string returns chars, which json/parse-string can't handle
  (map (comp json/parse-string str) ln))

(defn max-joltage [batts] (apply max batts))

(defn after [v ary] (rest (drop-while #(< % v) ary)))

(defn sum-joltage [v1 v2] (+ (* 10 v1) v2))

(defn find-joltage [batts]
  (let [mj (max-joltage batts)
        rs (after mj batts)]

    (if (empty? rs) 
      (sum-joltage (max-joltage (butlast batts)) mj)
      (sum-joltage mj (max-joltage (after mj batts))))))
  
(prn
  (->> *lines*
      (map parse-line)
      (map find-joltage)
      (reduce + 0)))
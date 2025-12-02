#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./one.bb
(def *lines* (line-seq (io/reader *in*)))


(defn invalid-id? [id]
  (let [id-str (str id)]
    (if (odd? (count id-str))
      false
      (s/ends-with? id-str (subs id-str 0 (/ (count id-str) 2))))))


(defn parse-num [s] (json/parse-string s))


(defn parse-range [s]
  (let [[start end] (s/split s #"-")]
    (range (parse-num start) (inc (parse-num end)))))


(defn parse-line [s]
  (mapcat parse-range (s/split s #",")))


(def result
  (->> *lines*
       first
       parse-line
       (filter invalid-id?)
       (reduce + 0)))

(prn result)


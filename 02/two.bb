#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))


(defn invalid-id? [id]
  ; use a cheap trick here: doubling the string and checking for repeats
  ; after index zero is a clever way to look for repeats
  (let [id-str (str id)
        idx (s/index-of (str id id) id-str 1)]
    (< idx (count id-str))))


(defn parse-num [s] (json/parse-string s))


(defn parse-range [s]
  (let [[start end] (s/split s #"-")]
    (range (parse-num start) (inc (parse-num end)))))


(defn parse-line [s]
  (mapcat parse-range (s/split s #",")))


(def result
  (->> *lines*
       first  ;; input has only one line!
       parse-line
       (filter invalid-id?)
       (reduce + 0)))

(prn result)


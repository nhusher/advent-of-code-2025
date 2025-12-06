#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))

(defn file-cols [lns]
  (let [row-chs (map vec lns)
        row-len (count (first row-chs))]
    (for [x (range row-len)] (map #(nth % x nil) row-chs))))

(defn empty-col? [col] (every? #(= \space %) col))

(defn split-col-groups [cols]
  (->> cols
       (partition-by empty-col?)
       (remove #(empty-col? (first %)))))

(defn parse-num [num-chs]
  (->> num-chs
      butlast
      (s/join "")
      s/trim
      parse-double
      int))

(defn parse-op [grp] ({ \+ +, \* * } (last (first grp))))

(defn parse-group [grp]
  { :expr (parse-op grp)
    :nums (reverse (map parse-num grp)) })

(defn eval-problem [p]
  (apply (:expr p) (:nums p)))

(defn parse-file [lns]
  (let [cols (file-cols lns)]
    (split-col-groups cols)))

(prn
  (->> *lines*
       parse-file
       (map parse-group)
       (map eval-problem)
       (reduce + 0)))

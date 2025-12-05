#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./one.bb
(def *lines* (line-seq (io/reader *in*)))

(defn parse-id [id] (-> id parse-double biginteger))
(defn parse-range [rng] (map parse-id (s/split rng #"-")))

(defn in-range? [rng v] 
  (let [[start end] rng]
    (<= start v end)))

(defn in-ranges? [rngs v]
  (or (some #(in-range? % v) rngs) false))

(let [[ranges _ ids]   (partition-by empty? *lines*)
      ingredient-ids   (map parse-id ids)
      fresh-ranges     (map parse-range ranges)
      in-input-ranges? (partial in-ranges? fresh-ranges)]

  (prn (->> ingredient-ids
       (filter in-input-ranges?)
       count)))
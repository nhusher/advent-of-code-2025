#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./one.bb
(def *lines* (line-seq (io/reader *in*)))

;; vecs here because we want random access with nth
(defn parse-line [ln] (vec (map #(if (= \@ %) :roll nil) ln)))
(defn parse-grid [lns] (vec (map parse-line *lines*)))

(defn in-bounds? [grid x y]
  (cond
    (neg? y)                    false
    (neg? x)                    false
    (>= y (count grid))         false
    (>= x (count (first grid))) false
    :else true))

(defn get-loc [grid x y]
  (when (in-bounds? grid x y) (-> grid (nth y) (nth x))))

(defn get-9-box [grid x y]
  (for [ys (range (dec y) (+ y 2))
        xs (range (dec x) (+ x 2))]
    (get-loc grid xs ys)))

(defn get-9-box-paper-ct [grid x y]
  (->> (get-9-box grid x y)
       (filter some?) ;; count anything that's not a nil
       count))
       
(defn get-roll-neighbor-cts [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (first grid)))]
    (when (get-loc grid x y) (dec (get-9-box-paper-ct grid x y)))))

(prn
  (->> *lines*
     parse-grid
     get-roll-neighbor-cts
     (filter some?)
     (filter #(> 4 %))
     count))
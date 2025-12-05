#!/usr/bin/env bb -i

(require '[clojure.string :as s])

;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))

(defn parse-id [id] (-> id parse-double biginteger))
(defn parse-range [rng] (map parse-id (s/split rng #"-")))
(defn sort-ranges [ranges] (sort-by first ranges))

(defn joinable? [range1 range2]
  (let [[r1s r1e] range1
        [r2s r2e] range2]
    (or 
      (and (<= r1s r2s) (>= r1e r2e))  ;; rng1 is superset
      (and (<= r2s r1s) (>= r2e r1e))  ;; rng2 is superset
      (and (>= r1e r2s) (<= r1s r2e))  ;; overlap right
      (and (>= r2e r1s) (<= r2s r1e))  ;; overlap left
      (= (inc r1e) r2s)  ;; neighbor right
      (= (inc r2e) r1s) ;; neighbor left
    )))

(defn any-range-joinable? [ranges]
  (let [rs   (for [a ranges b ranges] [a b])
        pred (fn [[a b]] (and (not= a b) (joinable? a b)))]
    (or (some pred rs) false)))

(defn join-ranges [range1 range2]
  (let [[r1s r1e] range1
        [r2s r2e] range2]
    (if (joinable? range1 range2)
      [[(min r1s r2s) (max r1e r2e)]]
      [range1 range2])))

(defn join-reducer [ranges rng]
  (if (empty? ranges) [rng]
      (concat (butlast ranges) (join-ranges (last ranges) rng))))

(defn join-all-ranges [ranges]
  (loop [rs ranges]
    (if-not (any-range-joinable? rs) rs
      (recur (reduce join-reducer [] rs)))))
      
(defn sum-all-ranges [ranges]
  (let [r (fn [acc [start end]] (+ acc 1 (- end start)))]
    (reduce r 0 ranges)))

(defn pr-ranges [ranges]
  (doseq [r (sort-ranges ranges)]
    (println (format "%d-%d" (first r) (second r)))))

(let [[ranges _ ids]   (partition-by empty? *lines*)
      fresh-ranges     (->> ranges (map parse-range) sort-ranges)
      joined-ranges    (join-all-ranges fresh-ranges)]
  (pr-ranges joined-ranges)
  (prn (sum-all-ranges joined-ranges)))

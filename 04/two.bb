#!/usr/bin/env bb -i

(require '[clojure.string :as s])
(require '[clojure.walk   :as w])

;; cat input.txt | ./two.bb
(def *lines* (line-seq (io/reader *in*)))

(defn parse-line [ln] (map #(if (= \@ %) :roll nil) ln))

(defn parse-grid [lns]
  ;; vec here because we want random access with nth and removal later on
  (let [cells (vec (mapcat parse-line *lines*))]
    { :cells  cells
      :height (count lns)
      :width  (count (first lns)) }))

(defn in-bounds? [grid x y]
  (cond
    (or (neg? y) (>= y (:height grid))) false
    (or (neg? x) (>= x (:width  grid))) false
    :else true))

(defn get-loc [grid x y] 
  (let [{:keys [cells height width]} grid
        idx (+ (* y height) x)]
    (when (in-bounds? grid x y) (nth cells idx))))

(def neighbor-offsets
  [[-1 -1] [0 -1] [1 -1]
   [-1  0]        [1  0]
   [-1  1] [0  1] [1  1]])

(defn neighbors [cell x y grid]
  (let [neighbor (fn [[xo yo]] (get-loc grid (+ x xo) (+ y yo)))]
    (when (in-bounds? grid x y)
      (w/walk neighbor vec neighbor-offsets))))

(defn removable? [cell x y grid]
  (when cell (->> (neighbors cell x y grid) (filter some?) count (> 4))))

(defn alter-grid [f grid]
  (loop [idx 0
         cells (transient (:cells grid))]
    (let [l (count cells)
          x (mod idx (:width grid))
          y (quot idx (:width grid))]
      (if-not (< idx l) (assoc grid :cells (persistent! cells))
        (recur (inc idx) (assoc! cells idx (f (nth cells idx) x y grid)))))))

(defn remove-rolls-when-possible [cell x y grid]
  (when-not (removable? cell x y grid) cell))

(defn any-removable? [grid]
  (let [x (fn [idx] (mod idx (:width grid)))
        y (fn [idx] (quot idx (:width grid)))
        f (fn [idx cell] (removable? cell (x idx) (y idx) grid))]
    (->> (:cells grid) (map f (range)) (some true?))))

(defn remove-all-possible-rolls [grid]
  (loop [grid' grid]
    (if-not (any-removable? grid') grid'
      (recur (alter-grid remove-rolls-when-possible grid')))))

(defn count-removed-rolls [before after]
  (let [ctr (fn [grid] (->> grid :cells (filter some?) count))]
    (- (ctr before) (ctr after))))

(defn pr-grid [grid]
  (let [cpt (fn [cell & _] (if (keyword? cell) "@" " "))
        {:keys [cells width]} (alter-grid cpt grid)
        lns (partition width cells)]
    (doseq [ln lns]
      (prn (s/join "" ln)))))

(let [grid (parse-grid *lines*)
      removed (remove-all-possible-rolls grid)]
  (pr-grid removed)
  (prn "removed" (count-removed-rolls grid removed) "rolls"))


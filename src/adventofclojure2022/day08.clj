(ns adventofclojure2022.day08
  (:require [clojure.string :as str :refer [split split-lines includes?]]))

(def raw-data (slurp "resources/data/day08/input.txt"))

(defn gen-twod-array [matrixdata]
  (let [lines (split-lines matrixdata)]
    (to-array-2d (mapv #(mapv (fn [n] (Integer/parseInt n)) (split % #"")) lines))))

(defn is-visible [mx x y]
  (let [treeval (aget mx x y)
        maxx (alength mx)
        maxy (alength (aget mx 0))]
    (or (= x 0)
        (= y 0)
        (= x (dec maxx))
        (= y (dec maxy))
        (every? #(> treeval (aget mx % y)) (range 0 x))
        (every? #(> treeval (aget mx % y)) (range (inc x) maxx))
        (every? #(> treeval (aget mx x %)) (range 0 y))
        (every? #(> treeval (aget mx x %)) (range (inc y) maxx)))))

(defn scenic-score [mx givenx giveny]
  (let [curtree (aget mx givenx giveny)
        maxx (alength mx)
        maxy (alength (aget mx 0))
        sides (conj []
                    (loop [xv (range (dec givenx) -1 -1)
                           ct 0]
                      (cond (nil? (first xv)) ct
                            (>= (aget mx (first xv) giveny) curtree) (inc ct)
                            :else (recur (rest xv) (inc ct))))
                    (loop [xv (range (inc givenx) maxx)
                           ct 0]
                      (cond (nil? (first xv)) ct
                            (>= (aget mx (first xv) giveny) curtree) (inc ct)
                            :else (recur (rest xv) (inc ct))))
                    (loop [yv (range (dec giveny) -1 -1)
                           ct 0]
                      (cond  (nil? (first yv)) ct
                             (>= (aget mx givenx (first yv)) curtree) (inc ct)
                             :else (recur (rest yv) (inc ct))))
                    (loop [yv (range (inc giveny) maxy)
                           ct 0]
                      (cond (nil? (first yv)) ct
                            (>= (aget mx givenx (first yv)) curtree) (inc ct)
                            :else (recur (rest yv) (inc ct)))))]
    (apply * sides)))

(defn part1 [givenstr]
  (let [mx (gen-twod-array givenstr)]
    (count (filter true?
                   (flatten (for [x (range 0 (alength mx))]
                              (for [y (range 0 (alength (aget mx 0)))]
                                (is-visible mx x y))))))))

(defn part2 [givenstr]
  (let [mx (gen-twod-array givenstr)
        coords (apply concat (for [x (range 0 (alength mx))]
                               (for [y (range 0 (alength (aget mx 0)))]
                                 [x y])))]
    (reduce (fn [mxscore [x y]]
              (let [ss (scenic-score mx x y)]
                (if (> ss mxscore)
                  ss
                  mxscore)))
            0 coords)))

(comment
  (part1 raw-data)
  (part2 raw-data)

  #_endcomment)

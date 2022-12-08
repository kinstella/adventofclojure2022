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

(defn part1 [givenstr]
  (let [mx (gen-twod-array givenstr)]
    (count (filter true?
                   (flatten (for [x (range 0 (alength mx))]
                              (for [y (range 0 (alength (aget mx 0)))]
                                (is-visible mx x y))))))))

(comment

  (part1 raw-data)

  #_endcomment)

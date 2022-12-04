(ns adventofclojure2022.day04
  (:require [clojure.string :as str :refer [split split-lines]]))

(def input-data (slurp "resources/data/day04/input.txt"))

(defn min-max [pair]
  (let [[mn mx] (split pair #"-")]
    [(Integer/parseInt mn)
     (Integer/parseInt mx)]))

(defn contain-pair [ln]
  (let [[lpair rpair] (split ln #",")]
    (let [[lmin lmax] (min-max lpair)
          [rmin rmax] (min-max rpair)]
      (or (and (<= lmin rmin) (>= lmax rmax))
          (and (<= rmin lmin) (>= rmax lmax))))))

(defn overlaps [ln]
  (let [[lpair rpair] (split ln #",")]
    (let [sorted (sort [(min-max lpair) (min-max rpair)])]
      (>= (last (first sorted))
          (first (last sorted))))))

(defn part01 []
  (let [lines (split-lines input-data)]
    (count (filter contain-pair lines))))

(defn part02 []
  (let [lines (split-lines input-data)]
    (count (filter overlaps lines))))

(comment
  (part01)
  (part02)
  #_endcomment)
(ns adventofclojure2022.day03
  (:require [clojure.string :as str :refer [split split-lines]]
            [clojure.set :refer [intersection]]))

(def input-data (slurp "resources/data/day03/input.txt"))

(defn score-char [lt]
  (let [ascii (int lt)]
    (if (and (>= ascii 65) (<= ascii 91))
      (- ascii 38)
      (- ascii 96))))

(defn process-line [ln]
  (let [[a b] (split-at (quot (count ln) 2) ln)
        common (set (filter (fn [el]
                              (some #(= % el) b)) a))]
    (println common)
    (reduce + (mapv score-char common))))

(defn day03-part01 []
  (let [lines (split-lines input-data)]
    (reduce + (mapv process-line lines))))

(defn find-badge [sacks]
  (let [[elf1 elf2 elf3] sacks
        badge (intersection (set elf1) (set elf2) (set elf3))]
    (first badge)))

(defn day03-part02 []
  (let [lines (split-lines input-data)
        elf-groups (partition 3 lines)
        badges (mapv find-badge elf-groups)]
    (reduce + (mapv score-char badges))))

(comment
  (day03-part01)
  (day03-part02) 
  #_endcomment)
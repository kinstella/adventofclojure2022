(ns adventofclojure2022.day02
  (:require [clojure.string :as str :refer [split split-lines]]))

(def input-data (slurp "resources/data/day02/data.txt"))

(def shape-points {"X" 1 ; rock
                   "Y" 2 ; paper
                   "Z" 3}) ; scissors

(def wins {"A" "Y" ; Y paper beats A rock
           "B" "Z" ; Z scissors beats B paper
           "C" "X"}) ; X rock beats C scissors 

(def draws {"A" "X"
            "B" "Y"
            "C" "Z"})

(def losses {"A" "Z" ; A rock
             "B" "X" ; B paper 
             "C" "Y"}) ; C scissors

(defn calc-hand [p1 p2]
  (let [score (cond (= p2 (get wins p1))
                    6
                    (= p2 (get draws p1))
                    3
                    :else
                    0)
        spoints (get shape-points p2)]
    (+ spoints score)))

(defn get-p2-play [p1 result]
  (cond (= result "X") ; lose
        (get losses p1)
        (= result "Y") ; draw
        (get draws p1)
        (= result "Z") ; win
        (get wins p1)))

(defn day02-part01 []
  (let [lines (split-lines input-data)]
    (reduce + (map #(let [[p1 p2] (split % #" ")]
                      (calc-hand p1 p2)) lines))))

(defn day02-part02 []
  (let [lines (split-lines input-data)]
    (reduce + (map  #(let [[p1 outcome] (split % #" ")
                           p2 (get-p2-play p1 outcome)]
                       (calc-hand p1 p2)) lines))))

(comment
  (day02-part01)
  (day02-part02)
  #_endcomment)
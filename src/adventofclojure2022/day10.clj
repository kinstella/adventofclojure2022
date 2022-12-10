(ns adventofclojure2022.day10
  (:require [clojure.string :as str :refer [split split-lines]]))

(def raw-data (slurp "resources/data/day10/input.txt"))

(def register (atom 1))
(def cycleidx (atom 0))
(def cyclevals (atom []))

(defn part1 [givendata]
  (let [lines (split-lines givendata)]
    (mapv #(let [[op val] (split % #"\s+")]
             (cond (= op "addx")
                   (do
                     (swap! cycleidx inc)
                     (swap! cyclevals conj [@cycleidx @register])
                     (swap! cycleidx inc)
                     (swap! cyclevals conj [@cycleidx @register])
                     (reset! register (+ @register (Integer/parseInt val))))
                   (= op "noop")
                   (do
                     (swap! cycleidx inc)
                     (swap! cyclevals conj [@cycleidx @register]))))
          lines)
    (let [endvals (mapv
                   #(last (filter (fn [[k v]]
                                    (= % k)) @cyclevals))
                   [20 60 100 140 180 220])]
      (apply + (mapv (fn [[c r]]
                       (* c r)) endvals)))))

(comment
  (part1 raw-data)

  #_endcomment)

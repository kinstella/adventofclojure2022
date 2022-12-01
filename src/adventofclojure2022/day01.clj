(ns adventofclojure2022.day01
  (:require [clojure.string :as str :refer [split split-lines]]))

(def input-data (slurp "resources/data/day01/data-part1.txt"))

(defn sum-each [d]
  (let [elfcals (split d #"\n\n")]
    (mapv
     (fn [v]
       (->> (mapv #(Integer/parseInt %) v)
            (reduce +))) (mapv split-lines elfcals))))

(defn day01-pt01 [data]
  (let [summed (sum-each data)]
    (first (reverse (sort summed)))))

(defn day01-pt02 [data]
  (let [summed (sum-each data)]
    (reduce + (take-last 3 (sort summed)))))

(comment
  (day01-pt01 input-data)
  (day01-pt02 input-data)
  #_endcomment)
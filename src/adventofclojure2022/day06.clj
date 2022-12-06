(ns adventofclojure2022.day06)

(def givenstr (slurp "resources/data/day06/input.txt"))

(defn find-unique [ln givenstr]
  (loop [remstr givenstr
         i 0]
    (let [thegroup (take ln remstr)]
      (if (= ln (count (set thegroup)))
        {thegroup (+ ln i)}
        (recur (rest remstr)
               (inc i))))))

(defn part1 [givenstr]
  (find-unique 4 givenstr))

(defn part2 [givenstr]
  (find-unique 14 givenstr))

(comment
  givenstr
  (part1 givenstr)
  (part2 givenstr)
  #_endcomment)

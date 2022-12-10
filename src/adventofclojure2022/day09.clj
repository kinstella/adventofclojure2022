(ns adventofclojure2022.day09
  (:require [clojure.string :as str :refer [split split-lines]]))

(def raw-data (slurp "resources/data/day09/input.txt"))

(def visited (atom []))
(def tailat (atom {:x 0 :y 4}))
(def headat (atom {:x 0 :y 4}))

(defn move-tail-to-head []
  (loop [xdif (- (:x @headat) (:x @tailat))
         ydif (- (:y @headat) (:y @tailat))]
    (if (and (<= (Math/abs xdif) 1) (<= (Math/abs ydif) 1))
      (swap! visited conj @tailat)
    ;; otherwise, keep shifting the values
      (do
        (if (>= (Math/abs xdif) 1)
          (do (if (neg? xdif)
                (swap! tailat update :x dec)
                (swap! tailat update :x inc))))

        (if (>= (Math/abs ydif) 1)
          (do (if (neg? ydif)
                (swap! tailat update :y dec)
                (swap! tailat update :y inc))))
        (swap! visited conj @tailat)
        (recur  (- (:x @headat) (:x @tailat))
                (- (:y @headat) (:y @tailat)))))))


(defn move-head-to-pos [moveto]
  (loop [xdif (- (:x moveto) (:x @headat))
         ydif (- (:y moveto) (:y @headat))]
    (if (and (= (Math/abs xdif) 0) (= (Math/abs ydif) 0))
    ;; otherwise, keep shifting the values
      (move-tail-to-head)
      (do
        (if (> (Math/abs xdif) 0)
          (do (if (neg? xdif)
                (swap! headat update :x dec)
                (swap! headat update :x inc))))
        (if (> (Math/abs ydif) 0)
          (do (if (neg? ydif)
                (swap! headat update :y dec)
                (swap! headat update :y inc))))
        (move-tail-to-head)
        (recur  (- (:x moveto) (:x @headat))
                (- (:y moveto) (:y @headat)))))))

(defn move-head [d]
  (let [[dir ct] d
        movetopos (let [curheadpos @headat]
                    (cond (= dir "U")
                          (assoc curheadpos :y (- (:y @headat) ct))
                          (= dir "D")
                          (assoc curheadpos :y (+ (:y @headat) ct))
                          (= dir "R")
                          (assoc curheadpos :x (+ (:x @headat) ct))
                          (= dir "L")
                          (assoc curheadpos :x (- (:x @headat) ct))))]
    (move-head-to-pos movetopos)))

(defn show-grid
  "just shows the grid to doublecheck things are right."
  []
  (doseq [y (range 0 5)]
    (doseq [x (range 0 6)]
      (if (contains? (set @visited) {:x x :y y})
        (print "#")
        (print ".")))
    (print "\n")))

(defn part1 [givendata]
  (swap! visited conj @tailat)
  (let [moves (split-lines givendata)]
    (mapv #(let [[dir ct] (split % #"\s+")]
             ;(println dir ct)
             (move-head [dir (Integer/parseInt ct)])
             [dir (Integer/parseInt ct)]) moves))
  (count (set @visited)))


(comment
  (part1 raw-data)
  (count (set @visited))
  (show-grid)

  #_endcomment)

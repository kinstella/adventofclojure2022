(ns adventofclojure2022.day09
  (:require [clojure.string :as str :refer [split split-lines includes?]]))

(def raw-data (slurp "resources/data/day09/example.txt"))

(def visited (atom []))
(def tail-loc (atom {:x 0 :y 0}))
(def head-loc (atom {:x 0 :y 0}))

(defn head-tail-dist []
  (let [xdist  (- (:x @head-loc) (:x @tail-loc))
        ydist  (- (:y @head-loc) (:y @tail-loc))]
    (+ (Math/abs xdist) (Math/abs ydist))))

(defn move-head [d]
  (let [[dir ct] d]
    (cond (= dir "U")
          (swap! head-loc assoc :y (- (:y @head-loc) ct))
          (= dir "D")
          (swap! head-loc assoc :y (+ (:y @head-loc) ct))
          (= dir "R")
          (swap! head-loc assoc :x (+ (:x @head-loc) ct))
          (= dir "L")
          (swap! head-loc assoc :x (- (:x @head-loc) ct))))
  (println "curhedaloc: " @head-loc
           " curtailloc: " @tail-loc)
  (println "dist now: " (head-tail-dist))
  (if (> (head-tail-dist) 1)
    (do (println "head is too FAR! - now at:" @head-loc " tail is at:" @tail-loc)
        (let [xdist (- (:x @head-loc) (:x @tail-loc))
              ydist (- (:y @head-loc) (:y @tail-loc))]
          (swap! tail-loc assoc :x

                 (if (> (Math/abs xdist) 1)
                   (+ (- xdist) (:x @tail-loc))
                   (:x @tail-loc))
                 :y
                 (if (> (Math/abs ydist) 1)
                   (+ (- ydist) (:y @tail-loc))
                   (:y @tail-loc)))
          (println " tailloc to:" @tail-loc)))
    (println "Not too far - -curhedaloc: " @head-loc
             " curtailloc: " @tail-loc))
  (println "------------"))

(defn part1 [givendata]
  (let [moves (split-lines givendata)]
    (mapv #(let [[dir ct] (split % #"\s+")]
             (println "dir" dir "ct" ct)
             (move-head [dir (Integer/parseInt ct)])
             [dir (Integer/parseInt ct)]) moves)))

(comment

  (part1 raw-data)


  (def fakehead {:x 10 :y 10})
  (def faketail {:x 11 :y 11})

  (- (:x fakehead) (:y faketail))


  -4 - 4

  - 8

  #_endcomment)


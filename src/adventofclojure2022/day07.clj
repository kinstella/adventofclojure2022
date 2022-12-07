(ns adventofclojure2022.day07
  (:require [clojure.string :as str :refer [split split-lines includes?]]))

(def raw-data (slurp "resources/data/day07/input.txt"))

(def filesys (atom {})) ; a map representing our filesystem as discovered
(def curloc (atom [])) ; our current working directory, as a stack
(def dir-sums (atom {})) ; a map of dirs by pathname and their size

(def TOTAL_DISK_SIZE 70000000)
(def SPACE_NEEDED 30000000)

(defn process-line [ln]
  (cond
    (includes? ln "$ cd ..")
    (swap! curloc pop)
    (includes? ln "$ cd /")
    (reset! curloc ["/"])
    (includes? ln "$ cd")
    (let [[_ _ navto] (split ln #"\s+")]
      (swap! curloc conj navto))
    (includes? ln "dir")
    (let [[_ dirname] (split ln #" ")]
      (swap! filesys assoc-in (conj @curloc dirname)  {}))
    (not (includes? ln "$")) ;; is a file
    (let [[fsize fname] (split ln #"\s+")]
      (swap! filesys
             assoc-in (conj @curloc fname) fsize))))

(defn dirsize
  "dirsizes -- a flatted map of each directory and its size "
  [curdir]
  (let [curval (get-in @filesys curdir)
        cursize (reduce (fn [totalsize k]
                          (if (map? (get curval k)) ;; the item is a directory, so recurse..
                            (+ totalsize (dirsize (conj curdir k)))
                            ;else it's a file, so just inc by that file's size
                            (+ totalsize (Integer/parseInt (get curval k)))))
                        0 ; we assume zero to start with
                        (keys curval))]
    (swap! dir-sums assoc curdir cursize)
    cursize))

(defn part1 [givenstr]
  (reset! curloc [])
  (reset! filesys {})
  (let [lines (split-lines givenstr)]
    (mapv #(process-line %) lines))
  ;; build dirsums atom of directories and content size...
  (dirsize ["/"])
  ;; add up everything < 100000
  (apply + (mapv (fn [[k v]]
                   (if (< v 100000)
                     v
                     0)) @dir-sums)))

(defn part2 [givenstr]
  (reset! curloc [])
  (reset! filesys {})
  (let [lines (split-lines givenstr)]
    (mapv #(process-line %) lines))
  ;; build dirsums atom of directories and content size...
  (dirsize ["/"])
  ; find the directry that's smallest, but large enough to free the space we need
  (let [amount-needed (- SPACE_NEEDED (- TOTAL_DISK_SIZE (get @dir-sums ["/"])))]
    (reduce (fn [closest d]
              (let [cursize (get @dir-sums d)]
                (if (and (> cursize amount-needed) (< cursize closest))
                  cursize
                  closest)))
            TOTAL_DISK_SIZE
            (keys @dir-sums))))

(comment
  (part1 raw-data)
  (part2 raw-data)
  #_endcomment)

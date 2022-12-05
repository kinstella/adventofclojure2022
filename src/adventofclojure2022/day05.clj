(ns adventofclojure2022.day05
  (:require
   [clojure.string :as str :refer [split split-lines trim]]))

(def input-data (slurp "resources/data/day05/input.txt"))

(def stackatoms (atom [])) ; init for 2d array later

(defn unbox [crate]
  (clojure.string/replace (trim crate) #"[\[\]]" ""))

(defn push-to-stack [snum el]
  (if (> (count el) 0)
    (let [newarray (conj (nth @stackatoms snum) el)]
      (swap! stackatoms assoc snum newarray))))

(defn process-stack-line [ln]
  (let [all-stacks (re-seq #".{1,4}" ln)
        unboxed (mapv unbox all-stacks)]
    (doall (map-indexed
            (fn [i e]
              (push-to-stack i e))
            unboxed))))

(defn parse-stacks [stacks]
  (let [lines (reverse stacks)]
    (mapv process-stack-line
          (rest lines))))

(defn parse-step [step]
  (if (> (count step) 0)
    (let [justnums (-> step
                       (clojure.string/replace #"move" "")
                       (clojure.string/replace #"from" "")
                       (clojure.string/replace #"to" "")
                       trim)
          [ct fm to] (mapv #(Integer/parseInt %) (split justnums #"\s+"))]
      [ct fm to])))

(defn do-step [[ct mvfrom mvto]]
  (let [taken (take-last ct (nth @stackatoms mvfrom))]
    #_(reset! stack-1 (vec (drop-last ct @stack-1)))
    taken))

(defn part01 []
  (reset! stackatoms [])
  (let [[stacks moves] (split input-data #"\n\n")
        stackvec (split-lines stacks)
        moves (split-lines moves)
        stack-count (split (trim (last stackvec)) #"\s+")]
    ; init stacks
    (doseq [s stack-count]
      (swap! stackatoms conj []))
    (parse-stacks stackvec)

    ; process moves...
    (let [to-process (mapv parse-step moves)]
      (println to-process)))
  #_(println moves))

(comment
  (part01)

  @stackatoms



  ;
  #_endcomment)
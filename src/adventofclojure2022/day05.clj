(ns adventofclojure2022.day05
  (:require
   [clojure.string :as str :refer [split split-lines trim]]))

(def input-data (slurp "resources/data/day05/input.txt"))

(def stackatoms (atom [])) ; init for 2d array later

(defn push-to-stack [snum el]
  (if (> (count el) 0)
    (let [newarray (conj (nth @stackatoms snum) el)]
      (swap! stackatoms assoc snum (vec newarray)))))

(defn unbox [crate]
  (clojure.string/replace (trim crate) #"[\[\]]" ""))

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

(defn init-stacks [stack-ct stackvec]
  (doseq [s stack-ct]
    (swap! stackatoms conj []))
  (parse-stacks stackvec))

(defn parse-step [step]
  (if (> (count step) 0)
    (let [justnums (-> step
                       (clojure.string/replace #"move" "")
                       (clojure.string/replace #"from" "")
                       (clojure.string/replace #"to" "")
                       trim)
          [ct fm to] (mapv #(Integer/parseInt %) (split justnums #"\s+"))]
      [ct fm to])))

(defn do-step [[ct mvfrom mvto] allatonce?]
  (let [mvfrom (dec mvfrom)
        mvto (dec mvto)
        taken (take-last ct (nth @stackatoms mvfrom))
        newfrom (drop-last ct (nth @stackatoms mvfrom))]
    (swap! stackatoms assoc mvfrom (vec newfrom))

    (let [newto (into (nth @stackatoms mvto) (if allatonce?
                                               taken
                                               (reverse taken)))]
      (swap! stackatoms assoc mvto (vec newto)))))

(defn get-top-of-stacks []
  (apply str (for [x (range 0 (count @stackatoms))]
               (last (nth @stackatoms x)))))

(defn part01 []
  (reset! stackatoms [])
  (let [[stacks moves] (split input-data #"\n\n")
        stackvec (split-lines stacks)
        moves (split-lines moves)
        stack-count (split (trim (last stackvec)) #"\s+")]
    (init-stacks stack-count stackvec)
    ; process moves...
    (let [to-process (mapv parse-step moves)]
      (doseq [m to-process]
        (do-step m false))))
  (get-top-of-stacks))

(defn part02 []
  (reset! stackatoms [])
  (let [[stacks moves] (split input-data #"\n\n")
        stackvec (split-lines stacks)
        moves (split-lines moves)
        stack-count (split (trim (last stackvec)) #"\s+")]
    (init-stacks stack-count stackvec)
    ; process moves...
    (let [to-process (mapv parse-step moves)]
      (doseq [m to-process]
        (do-step m true)))
    (get-top-of-stacks)))

(comment
  (part01)
  (part02)

  #_endcomment)
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
      (swap! stackatoms assoc snum (vec newarray)))))

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
  (let [mvfrom (dec mvfrom)
        mvto (dec mvto)
        taken (take-last ct (nth @stackatoms mvfrom))
        newfrom (drop-last ct (nth @stackatoms mvfrom))]
    (swap! stackatoms assoc mvfrom (vec newfrom))

    (let [newto (into (nth @stackatoms mvto) (reverse taken))]
      (swap! stackatoms assoc mvto (vec newto)))))

(defn do-step-part-two [[ct mvfrom mvto]]
  (let [mvfrom (dec mvfrom)
        mvto (dec mvto)
        taken (take-last ct (nth @stackatoms mvfrom))
        newfrom (drop-last ct (nth @stackatoms mvfrom))]
    (swap! stackatoms assoc mvfrom (vec newfrom))

    (let [newto (into (nth @stackatoms mvto) taken)]
      (swap! stackatoms assoc mvto (vec newto)))))


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
      (println to-process)

      (doseq [m to-process]
        (do-step m))))
  ; print last of each stack
  (apply str (for [x (range 0 (count @stackatoms))]
               (last (nth @stackatoms x)))))

(defn part02 []
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
      (println to-process)

      (doseq [m to-process]
        (do-step-part-two m))))
  ; print last of each stack
  (apply str (for [x (range 0 (count @stackatoms))]
               (last (nth @stackatoms x)))))


(comment
  (part01)
  (part02)

  @stackatoms
  ;


  #_endcomment)
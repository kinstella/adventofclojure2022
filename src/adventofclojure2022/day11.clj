(ns adventofclojure2022.day11
  (:require [clojure.string :as str :refer [split split-lines trim]]))

(def raw-data (slurp "resources/data/day11/input.txt"))

(def mqs (atom {}))
(def lcm (atom 0))
(def is-part1 (atom true))

(defn op-f
  "returns a lambda that expects one param"
  [opstr]
  (let [[op arg] (split opstr #"\s+")]
    (if (= op "*")
      (if (= arg "old")
        #(* % %)
        #(* (Integer/parseInt arg) %))
      (if (= arg "old")
        #(+ % %)
        #(+ (Integer/parseInt arg) %)))))

(defn process-monkey [mid]
  (loop [items (get-in @mqs [mid :items])]
    (if (empty? items)
      (swap! mqs assoc-in [mid :items] [])
      ; else
      (do
        (swap! mqs update-in [mid :inspected] inc)
        (let [theop (get-in @mqs [mid :op])
              worry  (theop (first items))
              worry (if @is-part1
                      (quot worry 3)
                      (mod worry @lcm))]
          (let [throwto (if (= (mod worry (get-in @mqs [mid :divby])) 0)
                          (get-in @mqs [mid :true-to])
                          (get-in @mqs [mid :false-to]))]

        ; throw it to...
            (swap! mqs assoc-in [throwto :items] (conj (get-in @mqs [throwto :items]) worry)))
          (recur (rest items)))))))

(defn monkey-rec [monkey-data]
  (let [lines (split-lines monkey-data)
        [_ mid]  (split (first lines) #"\s+")
        [_ items] (split (nth lines 1) #": ")
        op (trim (str/replace (nth lines 2) #"Operation: new = old " ""))
        [_ divby] (split (nth lines 3) #"by ")
        truerecip (trim (str/replace (nth lines 4) #"If true: throw to monkey " ""))
        falserecip (trim (str/replace (nth lines 5) #"If false: throw to monkey " ""))]
    {:id (Integer/parseInt (str/replace mid #":" ""))
     :items (mapv #(Integer/parseInt (trim %))
                  (split items #","))
     :op (op-f op)
     :divby (Integer/parseInt divby)
     :true-to (Integer/parseInt truerecip)
     :false-to (Integer/parseInt falserecip)
     :inspected 0}))

(defn mult-two-highest []
  (reduce * (take-last 2 (sort (mapv
                                #(get-in @mqs [% :inspected])
                                (keys @mqs))))))

(defn get-lcm []
  (reduce * (mapv
             #(get-in @mqs [% :divby])
             (keys @mqs))))

(defn part-1 [givendata days]
  (let [monkeys (split givendata #"\n\n")
        mrecs (mapv #(monkey-rec %) monkeys)]
    (mapv #(swap! mqs assoc (:id %) %) mrecs)
    (reset! lcm (get-lcm))
    (mapv
     (fn [d]
       (doall
        (for [m (range 0 (count mrecs))]
          (process-monkey m)))) (range 0 days)))
  (mult-two-highest))

(comment
  (reset! is-part1 true)
  (part-1 raw-data 20)
  (reset! is-part1 false)
  (part-1 raw-data 10000)

  #_endcomment)
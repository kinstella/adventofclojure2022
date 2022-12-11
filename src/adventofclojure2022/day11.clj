(ns adventofclojure2022.day11
  (:require [clojure.string :as str :refer [split split-lines trim]]))

(def raw-data (slurp "resources/data/day11/example.txt"))

(def mqs (atom {}))

(defn op-f [opstr]
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
              ;_ (println "item is:" (first items))
              worry (bigint (theop (bigint (first items))))
              _ (println "worry is now:" worry)
              worry (quot worry 3)
              ;_ (println "and divided by 3 is now: " worry)
              ]
          #_(println "is " worry " divisible by " (get-in @mqs [mid :divby]) "?:" (= 0 (mod worry (get-in @mqs [mid :divby]))))
          (let [throwto (if (= (mod worry (get-in @mqs [mid :divby])) 0)
                          (get-in @mqs [mid :true-to])
                          (get-in @mqs [mid :false-to]))]
            ;(println "throwing to: " throwto)

        ; throw it to...
            (swap! mqs assoc-in [throwto :items] (conj (get-in @mqs [throwto :items]) worry))
            #_(println "should have added " worry " to queue for monkey " throwto " which is now: " (get-in @mqs [throwto :items])))

          #_(println "rest of items is:" (rest items))
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

(defn part-1 [givendata days]
  (let [monkeys (split givendata #"\n\n")
        mrecs (mapv #(monkey-rec %) monkeys)]
    (mapv #(swap! mqs assoc (:id %) %
                  #_(println "-> rec" %)) mrecs)
    (mapv
     (fn [d]
       (doall
        (for [m (range 0 (count mrecs))]
          (do
            ;(println "Monkey " m ": " (get-in @mqs [m :items]))
            (process-monkey m))
          #_(println "monkey:" m)))
       #_(println "------------ Day " (inc d) "--------------\n" @mqs)) (range 0 days)))

  (mult-two-highest))

(comment
  (part-1 raw-data 100)




  raw-data

  (get-in @mqs [0 :items])
  (swap! mqs assoc-in [0 :items] [])

  #_endcomment)
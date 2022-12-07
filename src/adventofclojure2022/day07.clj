(ns adventofclojure2022.day07
  (:require [clojure.string :as str :refer [split split-lines includes?]]))

(def givenstr (slurp "resources/data/day07/example.txt"))

(def filesys (atom {})) ; dir can be a nested map
(def curloc (atom [])) ; location can be a stack

(defn process-line [ln]
  (cond
    (includes? ln "$ cd ..")
    (swap! curloc pop)

    (includes? ln "$ cd /")
    (reset! curloc ["/"])

    (includes? ln "$ cd")
    (let [[_ _ navto] (split ln #"\s+")]
      (println "navto?:" navto)
      (swap! curloc conj navto))

    (includes? ln "$ ls")
    (println "just ls within " @curloc)

    (includes? ln "dir")
    (let [[_ dirname] (split ln #" ")]
      (println "Addming dir to: " @curloc " with name " dirname)
      (swap! filesys assoc-in (conj @curloc dirname)  {})
      (println "Current Loc:" @curloc)
      (println "Current Filesys:" @filesys)
      (println "-------------------------------"))

    :else ;; is a file
    (do
      (let [[fsize fname] (split ln #"\s+")]
        (println "Addming file to: " @curloc " with name " fname)
        (swap! filesys
               assoc-in (conj @curloc fname) fsize))
      (println "Current Loc:" @curloc)
      (println "Current Filesys:" @filesys)
      (println "-------------------------------"))))

(defn part1 [givenstr]
  (reset! curloc [])
  (reset! filesys {})
  (let [lines (split-lines givenstr)]
    (mapv #(process-line %) lines)))

(comment

  (part1 givenstr)


  (swap! curloc conj "/")

  @curloc
  @filesys
  ;; => {"/" {"d" {"k" "7214296"}, "a" {"h.lst" "62596", "e" {"i" "584"}}}}

  ;; => {"" [], "/" {"d" {"k" "7214296"}, "a" {"h.lst" "62596", "e" {"i" "584"}}}}

  (clojure.pprint/pprint @filesys)

  (swap! curloc conj "ok")

  #_endcomment)

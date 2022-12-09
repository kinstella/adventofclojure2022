(ns adventofclojure2022.day09)


(def mh (atom {:x 0 :y 0}))
(def mt (atom {:x 3 :y 2}))
(def tailtracks (atom []))


(defn move-tail-to-head []
  (loop [xdif (- (:x @mh) (:x @mt))
         ydif (- (:y @mh) (:y @mt))]
    (println "xdif: " xdif " ydif:" ydif)
    (if (and (<= (Math/abs xdif) 1) (<= (Math/abs ydif) 1))
      true
    ;; otherwise, keep shifting the values
      (do
        (if (> (Math/abs xdif) 1)
          (if (neg? xdif)
            (swap! mt update :x dec)
            (swap! mt update :x inc)))
        (if (> (Math/abs ydif) 1)
          (if (neg? ydif)
            (swap! mt update :y dec)
            (swap! mt update :y inc)))
        (println "pos of tail: " @mt)
        (swap! tailtracks conj @mt)
        (recur  (- (:x @mh) (:x @mt))
                (- (:y @mh) (:y @mt)))))))

(move-tail-to-head)
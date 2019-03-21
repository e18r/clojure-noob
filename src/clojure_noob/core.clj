(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))

(defn train
  []
  (println "Choo chooo!"))

(def asym-hobbit-body-parts
  [{:name "head" :size 3}
   {:name "left-eye" :size 1}
   {:name "left-ear" :size 1}
   {:name "mouth" :size 1}
   {:name "nose" :size 1}
   {:name "neck" :size 2}
   {:name "left-shoulder" :size 3}
   {:name "left-upper-arm" :size 3}
   {:name "chest" :size 10}
   {:name "back" :size 10}
   {:name "left-forearm" :size 3}
   {:name "abdomen" :size 6}
   {:name "left-kidney" :size 1}
   {:name "left-hand" :size 2}
   {:name "left-knee" :size 2}
   {:name "left-thigh" :size 4}
   {:name "left-lower-leg" :size 3}
   {:name "left-achilles" :size 1}
   {:name "left-foot" :size 2}])

(defn expand-body-parts
  "Expects a vector of maps with :name and :size"
  [expander body-parts]
  (reduce expander [] body-parts))

(defn symmetrizer
  "Creates right body parts out of left body parts"
  [final part]
  (if (re-find #"^left-" (:name part))
    (into final
          (set [part
                {:name (clojure.string/replace (:name part) #"^left-" "right-")
                 :size (:size part)}]))
    (conj final part)))

(defn spider-expand
  "Multiplies body parts"
  ([part]
   (spider-expand part [] 1))
  ([part final index]
   (if (> index 4)
     final
     (spider-expand part
                    (conj final
                          {:name (str (:name part) index)
                           :size (:size part)})
                    (inc index)))))

(defn spider-expander
  "Creates spider like bodies"
  [final part]
  (if (or (re-find #"-eye$" (:name part))
          (re-find #"-leg$" (:name part)))
    (into final
          (spider-expand part))
    (conj final part)))

(expand-body-parts spider-expander (expand-body-parts symmetrizer asym-hobbit-body-parts))

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

(defn hit
  [asym-body-parts]
  (let [sym-parts (expand-body-parts symmetrizer asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(#(+ % 100) 5)

(defn dec-maker
  "Creates an decrementer function"
  [n]
  (fn [x]
    (- x n)))

(def dec9 (dec-maker 9))
(dec9 10)

(defn mapset
  "works like `map` except the return value is a set"
  [f vector]
  (loop [head (first vector)
         tail (rest vector)
         result #{}]
    (if (nil? head)
      result
      (recur (first tail)
             (rest tail)
             (conj result
                   (f head))))))

(mapset inc [1 1 2 2])

(defn radialize
  "Multiplies body part according to radial symmetry"
  ([part]
   (radialize part [] 1))
  ([part result index]
   (if (> index 5)
     result
     (let [ordinals ["0st" "1st" "2nd" "3rd" "4th" "5th"]]
       (radialize part
                  (conj result
                        {:name (clojure.string/replace (:name part)
                                                       #"^left-"
                                                       (str (ordinals index) "-radial-"))
                         :size (:size part)})
                  (inc index))))))

(radialize {:name "left-arm" :size 4})

(defn radializer
  "Creates body parts for aliens with radial symmetry (5 parts instead of 2)"
  [final part]
  (if (re-find #"^left-" (:name part))
    (into final (radialize part))
    (conj final part)))

(radializer [] {:name "left-ear" :size 4})

(expand-body-parts radializer asym-hobbit-body-parts)

(defn add
  "Multiplies a part a given number of times"
  ([number part]
   (add number part [] 1))
  ([number part result index]
   (if (> index number)
     result
     (add number part
          (conj result
                {:name (str
                        (clojure.string/replace (part :name) #"^left-" "")
                        "-" index)
                 :size (part :size)})
          (inc index)))))

(add 8 {:name "left-ear" :size 3})

(defn create-adder
  "Creates an adder function with the given number of parts to add"
  [number]
  (fn [final part]
    (if (re-find #"^left-" (part :name))
      (into final (add number part))
      (conj final part))))

((create-adder 6) [] {:name "hola" :size 3})
((create-adder 6) [] {:name "left-hola" :size 3})

(defn add-body-parts
  "Takes a collection of body parts and adds a given number of body parts to it"
  [asym-body-parts number]
  (let [adder (create-adder number)]
    (reduce adder [] asym-body-parts)))

(add-body-parts asym-hobbit-body-parts 3)

(fn [seq nth]
  (loop [head (first seq)
         tail (rest seq)
         index 0]
    (if (= index nth)
      head
      (recur (first tail)
             (rest tail)
             (inc index)))))

(= (enth '(4 5 6 7) 2) 6)
(= (enth [:a :b :c] 0) :a)
(= (enth [1 2 3 4] 1) 2)
(= (enth '([1 2] [3 4] [5 6]) 2) [5 6])

(map inc [1 2 3 4])

(reduce str {:a 1 :b 2)}

(str {:a 1 :b 2})

(first #{:a :b})

(#(str %1 %2) "aou" "eee")

(map #(str (second %)) {:a 1})

(seq '(1 2 3))
(seq [1 2 3])
(seq #{1 2 3})
(seq {:name "Emi" :occupation "dev"})
(into {} (seq {:a 1 :b 2 :c 3}))

(map str ["a" "b" "c"])
(map str ["a" "b" "c"] ["A" "B" "C"])

(def human-consumption   [8.1 7.3 6.6 5.0])
(def critter-consumption [0.0 0.2 0.3 1.1])
(defn unify-diet-data
  [human critter]
  {:human human
   :critter critter})
(map unify-diet-data human-consumption critter-consumption)


(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats [numbers]
  (map #(% numbers) [sum count avg]))
(stats [3 4 10])
(stats [80 1 44 13 6])

(def identities
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Spider-Man" :real "Peter Parker"}
   {:alias "Santa" :real "Your mom"}
   {:alias "Easter Bunny" :real "Your dad"}])
(map :real identities)

(reduce (fn [new-map [key val]]
          (assoc new-map key (inc val)))
        {} {:max 30 :min 10})

(reduce (fn [new-map [key val]]
          (if (> val 4)
            (assoc new-map key val)
            new-map))
        {} {:human 4.1 :critter 3.9})

(defn mapea
  [f seq]
  (reduce (fn [new-seq elem]
            (conj new-seq
                  (f elem)))
          []
          seq))

(map inc [1 2 3])
(mapea inc [1 2 3])

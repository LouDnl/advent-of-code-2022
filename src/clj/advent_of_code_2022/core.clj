(ns advent-of-code-2022.core
  (:require [advent-of-code-2022.days.day-one :as dayone]
            [advent-of-code-2022.days.day-two :as daytwo]
            [advent-of-code-2022.days.day-three :as daythree]
            [advent-of-code-2022.days.day-four :as dayfour]
            [clojure.java.io :as io])
  (:gen-class))

#_(defn- clean-up
  []
  (clojure.core/remove-ns (quote advent-of-code-2022.core))
  (clojure.core/in-ns     (quote advent-of-code-2022.core))
  (clojure.core/require   '[clojure.core])
  (clojure.core/refer     'clojure.core)
  (use 'advent-of-code-2022.core :reload-all))

(defmacro adventreader
  [resource]
  `(let [data# (with-open
                [rdr# (io/reader ~resource)]
                 (doall (line-seq rdr#)))]
     data#))

(defn day-one-part-one
  "71300"
  []
  (let [resource "resources/day-one.txt"
        data     (adventreader resource)]
    (dayone/dayone-recur data)
    (println "Most calories:" (apply max @dayone/dayone-atom))))

(defn day-one-part-two
  "209691"
  []
  (when (empty? @dayone/dayone-atom)
    (day-one-part-one))
  (let [top-first  (reduce max @dayone/dayone-atom)
        top-second (reduce max (remove #{top-first} @dayone/dayone-atom))
        top-third  (reduce max (remove #{top-first top-second} @dayone/dayone-atom))
        total      (+ top-first top-second top-third)]
    (println (format "First: %s, Second: %s, Third: %s" top-first top-second top-third))
    (println "Total calories:" total)))

(defn day-two-part-one
  "14069"
  []
  (let [resource "resources/day-two.txt"
        data     (adventreader resource)
        rounds   (into [] data)]
    (daytwo/play rounds)))

(defn day-two-part-two
  "12411"
  []
  (let [resource "resources/day-two.txt"
        data     (adventreader resource)
        rounds   (into [] data)]
    (daytwo/play (daytwo/rigged-play rounds))))

(defn day-three-part-one
  "7875"
  []
  (let [resource    "resources/day-three.txt"
        data        (adventreader resource)
        rucksacks   (into [] data)
        prio-values (daythree/rucksack-item-priorities rucksacks)]
    (println "Total rucksacks:" (count rucksacks))
    (println "Sum of all priorities:" (reduce + prio-values))))

(defn day-three-part-two
  "2479"
  []
  (let [resource     "resources/day-three.txt"
        data         (adventreader resource)
        rucksacks    (into [] data)
        groupedelves (daythree/group-elves rucksacks)
        prio-values  (daythree/elvegroup-item-priorities groupedelves)]
    (println "Total elve groups:" (count groupedelves))
    (println "Sum of all priorities:" (reduce + prio-values))))

(defn day-four-part-one
  "431"
  []
  (println "Total overlapping groups:" (dayfour/count-overlapping-sections dayfour/input)))

(defn day-four-part-two
  "823"
  []
  (println "Total intersecting groups:" (dayfour/count-intersecting-sections dayfour/input)))

(defn -main
  "I run all days in random order if no argument is supplied

   Usage:
   * repl: (-main), (-main 'day-one-part-one) or (-main 'day-one-part-one 'day-one-part-two)
   * cli:  lein main, lein main day-one-part-one or lein main day-one-part-one day-one-part-two"
  [& args]
  (let [non-functions #{#'advent-of-code-2022.core/adventreader #'advent-of-code-2022.core/-main}
        function-map  (ns-publics 'advent-of-code-2022.core)
        all-functions (reverse (remove non-functions (set (vals function-map))))]
    (if (empty? args)
      (loop [[f & funcs] all-functions]
        (println (format "Starting function: %s" (-> f meta :name)))
        (f)
        (println)
        (when funcs (recur funcs)))
      (let [funcs args]
        (doall
         (for [f funcs
               :let [fun (get function-map (symbol f))]]
           (do (println (format "Starting function: %s" f))
               (fun)
               (println))))))))

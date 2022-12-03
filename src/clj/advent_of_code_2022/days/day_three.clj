(ns advent-of-code-2022.days.day-three
  (:require [clojure.set :refer [map-invert]]))

(def test-rucksacks
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   "CrZsJsPPZsGzwwsLwLmpwMDw"])

(def supply-value
  (map-invert (into {} (map-indexed
                        #(vector (inc %1) (str %2))
                        (flatten (cons (map char (range (int \a) (inc (int \z))))
                                       (map char (range (int \A) (inc (int \Z))))))))))

(defn compare-two-lists
  "Compares two lists and returns the distinct
   values that are in both lists"
  [listone listtwo]
  (distinct
   (for [i listone
         n listtwo
         :when (= i n)]
     i)))

(defn compare-three-lists
  "Compares three lists and returns the distinct
   values that are all three lists"
  [listone listtwo listthree]
  (distinct
   (for [i listone
         n listtwo
         y listthree
         :when (= i n y)]
     i)))

(defn rucksack-item-priorities
  [list-of-rucksacks]
  (let [items (flatten
               (for [rucksack list-of-rucksacks
                     :let [itemcount (count rucksack)
                           divided (/ itemcount 2)
                           compartment-one (map str (take divided rucksack))
                           compartment-two (map str (drop divided rucksack))]]
                 (compare-two-lists compartment-one compartment-two)))]
    (map #(get supply-value %) items)))

(defn group-elves
  [list-of-rucksacks]
  (let [elvegroups (loop [elvestoskip 0
                          groups []]
                     (let [elvestotake 3
                           group (take elvestotake (drop elvestoskip list-of-rucksacks))]
                       (if (>= (count group) elvestotake)
                         (recur (+ elvestoskip elvestotake) (conj groups (into [] group)))
                         (concat groups group))))]
    (vec elvegroups)))

(defn elvegroup-item-priorities
  [list-of-rucksacklists]
  (let [items (flatten
               (for [list-of-rucksacks list-of-rucksacklists
                     :let [rucksack-one   (map str (first list-of-rucksacks))
                           rucksack-two   (map str (second list-of-rucksacks))
                           rucksack-three (map str (last list-of-rucksacks))]]
                 (compare-three-lists rucksack-one rucksack-two rucksack-three)))]
    (map #(get supply-value %) items)))

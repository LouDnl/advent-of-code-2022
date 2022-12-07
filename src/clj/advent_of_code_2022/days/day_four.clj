(ns advent-of-code-2022.days.day-four
  (:require [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def testinput
  "content:
   2-4,6-8
   2-3,4-5
   5-7,7-9
   2-8,3-7
   6-6,4-6
   2-6,4-8"
  "resources/day-four-testinput.txt")

(def input "resources/day-four.txt")

;; Part one
(defn count-overlapping-sections
  "Returns the amount of completely overlapping sections"
  [data]
  (let [section-lines (string/split-lines
                       (slurp
                        (io/input-stream data)))
        intersecting (for [couple section-lines
                           :let [couples   (string/split couple #",")
                                 firstelf  (-> (first couples)
                                               (string/split #"-")
                                               (#(into #{} (range (parse-long (first %)) (+ (parse-long (last %)) 1)))))
                                 secondelf (-> (second couples)
                                               (string/split #"-")
                                               (#(into #{} (range (parse-long (first %)) (+ (parse-long (last %)) 1)))))
                                 difference (if (<= (count firstelf) (count secondelf))
                                              (diff firstelf secondelf)
                                              (when (<= (count secondelf) (count firstelf))
                                                (diff secondelf firstelf)))]
                           :when (nil? (first difference))]
                       true)]
    (count intersecting)))


;; Part two
(defn count-intersecting-sections
    "Returns the amount of partially overlapping sections"
    [data]
    (let [section-lines (string/split-lines
                         (slurp
                          (io/input-stream data)))
          overlapping (for [couple section-lines
                            :let [couples   (string/split couple #",")
                                  firstelf  (-> (first couples)
                                                (string/split #"-")
                                                (#(into #{} (range (parse-long (first %)) (+ (parse-long (last %)) 1)))))
                                  secondelf (-> (second couples)
                                                (string/split #"-")
                                                (#(into #{} (range (parse-long (first %)) (+ (parse-long (last %)) 1)))))
                                  difference (if (<= (count firstelf) (count secondelf))
                                               (diff firstelf secondelf)
                                               (when (<= (count secondelf) (count firstelf))
                                                 (diff secondelf firstelf)))]
                            :when (some? (last difference))]
                        true)]
      (count overlapping)))

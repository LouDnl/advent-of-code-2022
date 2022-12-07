(ns advent-of-code-2022.days.day-five
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            #_[clojure.data :refer [diff]]
            #_[clojure.edn :as edn]
            #_[clojure.set :as set]))

(def test-input
  "    [D]
   [N] [C]
   [Z] [M] [P]
    1   2   3

   move 1 from 2 to 1
   move 3 from 1 to 3
   move 2 from 2 to 1
   move 1 from 1 to 2"
  "resources/day-five-testinput.txt")

(def input "resources/day-five.txt")

(defn read-input
  "reads input data and returns a drawing of the moves and the procedure"
  [data]
  (let [parsed (string/split-lines
                (slurp
                 (io/input-stream data)))]
    (split-at (.indexOf parsed "") parsed)))

(defn rows-to-columns
  "Changes rows to columns, e.g. flips the input 90 degrees to the right

   Returns: {key value} where key is the column number and value contains all top down strings"
  [box-columns max-columns & {:keys [columns] :or {columns nil}}]
  (let [max-columns (if (seq? max-columns)
                      max-columns
                      (range 1 (inc max-columns)))
        first-column (doall (for [row box-columns
                                  :let [columnid (first max-columns)
                                        column (map str row)]
                                  :when (seq column)]
                              {columnid (first column)}))
        previous-columns columns
        new-columns {(first (distinct (flatten (map keys first-column)))) (apply str (flatten (map vals first-column)))}
        _columns (merge-with into previous-columns new-columns)
        rest (doall
              (for [row box-columns
                    :let [column (drop 1 row)]
                    :when (seq column)]
                (apply str column)))]
    (if (> (count rest) 0)
      (rows-to-columns rest (drop 1 max-columns) :columns _columns)
      _columns)))

(defn cratemover-9000
  "Moves one box at a time"
  [[drawing procedure]]
  (let [boxes-bottom (.indexOf drawing (last drawing)) ;=> 3
        box-columns (take (+ boxes-bottom 2) drawing) ;=> ("    [D]" "[N] [C]" "[Z] [M] [P]" " 1   2   3")
        moves-list (drop 1 procedure) ;=> ("move 1 from 2 to 1" "move 3 from 1 to 3" "move 2 from 2 to 1" "move 1 from 1 to 2")
        moves (mapv (fn [n] (->> n (re-seq #"\d{1,2}") (map parse-long) vec)) moves-list) ;=> [[1 2 1] [3 1 3] [2 2 1] [1 1 2]]
        bottom-row (first ;=> "[Z] [M] [P]"
                    (take-last 2 drawing)) ;=? ("[Z] [M] [P]" " 1   2   3")
        max-columns (count bottom-row) ;=> 11
        boxmap (rows-to-columns box-columns max-columns)
        mapped-boxes (sort-by first ;=> ({1 "NZ"} {5 "DCM"} {9 "P"})
                              (filter identity
                                      (map (fn [[k v]]
                                             (when-let [vv (re-find #"[A-Z0-9].*" v)]
                                               {k vv}))
                                           boxmap)))
        mapped-boxes (into {} ;=> {1 "NZ", 2 "DCM", 3 "P"}
                           (map (fn [[_k v]]
                                  {(parse-long (str (last v))) (apply str (flatten (map str (drop-last v))))})
                                (into {} (flatten mapped-boxes))))
        outcome (loop [[m & _moves :as mm] moves ; returns an unsorted map
                       result mapped-boxes] ; start {1 "NZ", 2 "DCM", 3 "P"}
                  (if mm
                    (recur _moves
                           (merge result
                                  (let [move (first m)
                                        from (second m)
                                        to (last m)]
                                    {to ; key
                                     ; new to value
                                     (apply str (concat
                                                 (apply str (reverse (take move (get result from)))) ; takes one box at a time (in fact takes all but reverses the taken boxes as if one)
                                                 (get result to)))
                                     from ; key
                                     ; new from value
                                     (apply str (drop move (get result from)))})))
                    result)) ;=> end result {2 "M", 1 "C", 3 "ZNDP"}
        outcome-sorted (sort-by key outcome) ;=> ([1 "C"] [2 "M]" [3 "ZNDP"]})
        result (apply str (map (fn [[_k v]]
                                 (first v)) outcome-sorted))] ;=> "CMZ"
    result))

(defn cratemover-9001
  "Moves multiple boxes at a time"
  [[drawing procedure]]
  (let [boxes-bottom (.indexOf drawing (last drawing)) ;=> 3
        box-columns (take (+ boxes-bottom 2) drawing) ;=> ("    [D]" "[N] [C]" "[Z] [M] [P]" " 1   2   3")
        moves-list (drop 1 procedure) ;=> ("move 1 from 2 to 1" "move 3 from 1 to 3" "move 2 from 2 to 1" "move 1 from 1 to 2")
        moves (mapv (fn [n] (->> n (re-seq #"\d{1,2}") (map parse-long) vec)) moves-list) ;=> [[1 2 1] [3 1 3] [2 2 1] [1 1 2]]
        bottom-row (first ;=> "[Z] [M] [P]"
                    (take-last 2 drawing)) ;=? ("[Z] [M] [P]" " 1   2   3")
        max-columns (count bottom-row) ;=> 11
        boxmap (rows-to-columns box-columns max-columns)
        mapped-boxes (sort-by first ;=> ({1 "NZ"} {5 "DCM"} {9 "P"})
                              (filter identity
                                      (map (fn [[k v]]
                                             (when-let [vv (re-find #"[A-Z0-9].*" v)]
                                               {k vv}))
                                           boxmap)))
        mapped-boxes (into {} ;=> {1 "NZ", 2 "DCM", 3 "P"}
                           (map (fn [[_k v]]
                                  {(parse-long (str (last v))) (apply str (flatten (map str (drop-last v))))})
                                (into {} (flatten mapped-boxes))))
        outcome (loop [[m & _moves :as mm] moves ; returns an unsorted map
                       result mapped-boxes] ; start {1 "NZ", 2 "DCM", 3 "P"}
                  (if mm
                    (recur _moves
                           (merge result
                                  (let [move (first m)
                                        from (second m)
                                        to (last m)]
                                    {to ; key
                                     ; new to value
                                     (apply str (concat
                                                 (apply str (take move (get result from))) ; takes all boxes at once
                                                 (get result to)))
                                     from ; key
                                     ; new from value
                                     (apply str (drop move (get result from)))})))
                    result)) ;=> end result {1 "M", 2 "C", 3 "DNZP"}
        outcome-sorted (sort-by key outcome) ;=> ([1 "M"] [2 "C"] [3 "DNZP"])
        result (apply str (map (fn [[_k v]]
                                 (first v)) outcome-sorted))] ;=> "MCD"
    result))

#_(comment "rotating the input"

         (let [data (string/split-lines
                     (slurp
                      (io/input-stream test-input)))
               empty-row (.indexOf data "") ;=> 4
               boxes-bottom (- empty-row 2) ;=> 2
               nrows (range 0 (+ boxes-bottom 1)) ;=> (0 1 2)
               ;box-columns (take (+ boxes-bottom 1) data) ;=> ("    [D]" "[N] [C]" "[Z] [M] [P]")
               box-columns (take (+ boxes-bottom 2) data) ;=> ("    [D]" "[N] [C]" "[Z] [M] [P]" " 1   2   3")
               column-count (- empty-row 1) ;=> ("    [D]" "[N] [C]" "[Z] [M] [P]")
               ;box-columns (take column-count data) ;=> 3
               moves-start (+ empty-row 1) ;=> 5
               empty-column [\space \space \space] ;=> [\space \space \space]
               space 32
               columns (mapv #(parse-long (str %)) (remove #(= 32 (int %)) (seq (get data column-count)))) ;=> [1 2 3]
               moves-list (mapv #(get data %) (range moves-start (count data))) ;=> ["move 1 from 2 to 1" "move 3 from 1 to 3" "move 2 from 2 to 1" "move 1 from 1 to 2"]
               moves (map #(re-seq #"\d" %) moves-list) ;=> (("1" "2" "1") ("3" "1" "3") ("2" "2" "1") ("1" "1" "2"))
               movesv2 (mapv (fn [n] (->> n (re-seq #"\d") (map parse-long) vec)) moves-list) ;=> [[1 2 1] [3 1 3] [2 2 1] [1 1 2]]
               longest-row (apply max (mapv #(count %) (take column-count data))) ;=> 11
               bottom-row (count (get data boxes-bottom)) ;=> 11
               boxmap (rows-to-columns box-columns bottom-row)
               boxtypes (map str (map char (range (int \A) (inc (int \Z)))))
               mapped-boxes (sort-by first ;=> ({1 "NZ"} {5 "DCM"} {9 "P"})
                                     (filter identity
                                             (map (fn [[k v]]
                                                    (when-let [vv (re-find #"[A-Z0-9].*" v)]
                                                      {k vv}))
                                                  boxmap)))
               mapped-boxes (into {} ;=> {1 "NZ", 2 "DCM", 3 "P"}
                                  (map (fn [[_k v]]
                                         {(parse-long (str (last v))) (apply str (flatten (map str (drop-last v))))})
                                       (into {} (flatten mapped-boxes))))
               new-row-range (range 1 (inc (count mapped-boxes)))
               outcome (loop [[m & _moves :as mm] movesv2
                              result mapped-boxes] ; start {1 "NZ", 2 "DCM", 3 "P"}
                         (if mm
                           (recur _moves
                                  (merge result
                                         ;(prn "boxes" result)
                                         ;(prn "m" m) ; [1 2 1] => always the current match
                                         ;(prn "_moves" _moves) ; ([3 1 3] [2 2 1] [1 1 2]) => nil => matches without current match until empty
                                         ;(prn "mm" mm) ;  [[1 2 1] [3 1 3] [2 2 1] [1 1 2]] => ([1 1 2]) => all matches until the last
                                         (let [move (first m)
                                               from (second m)
                                               to (last m)]
                                           ; The following should be the result of this loop/recur
                                           ; moves: [1 2 1] {1 "NZ", 2 "DCM", 3 "P"} => "DCM" => take 1 \D => \D => "D" => "DNZ"
                                           ; moves: [3 1 3] {1 "DNZ", 2 "CM", 3 "P"} => "DNZ" => take 3 \D\N\Z => \Z\N\D => "ZND" => "ZNDP"
                                           ; moves: [2 2 1] {1 "", 2 "CM", 3 "ZNDP"} => "CM" => take 2 \C \M => \M \C => "MC" => "MC"
                                           ; moves: [1 1 2] {1 "MC", 2 "", 3 "ZNDP"} => "MC" => take 1 \M => \M => "M" => "M"
                                           ; result: {1 "C", 2 "M", 3 "ZNDP"}
                                           {to ; key
                                            ; new to value
                                            (apply str (concat
                                                        (apply str (reverse (take move (get result from))))
                                                        (get result to)))

                                            from ; key
                                            ; new from value
                                            (apply str (drop move (get result from)))})))
                           result))] ; end result {1 "C", 2 "M", 3 "ZNDP"}
           (apply str (map (fn [[_k v]]
                             (first v)) outcome)))
         )

#_(comment "conversion tries"

  (defn rows-to-columnsv2
    [rows max-char]
    (let [char-range (range  max-char)
          box-columns-all  (doall (for [chr char-range
                                        row rows]
                                    {chr (apply str (take 1 (drop chr row)))}))
          box-columms (apply merge-with str box-columns-all)
          boxtypes (map str (map char (range (int \A) (inc (int \Z)))))]
      rows))
  )

#_(comment "move testing"

         (let [_mymap {0 " [[",
                       1 " NZ",
                       2 " ]]",
                       3 "   ",
                       4 "[[[",
                       5 "DCM",
                       6 "]]]",
                       7 " ",
                       8 "[",
                       9 "P",
                       10 "]"}
               _mymap {1 " [[",
                       2 " NZ",
                       3 " ]]",
                       4 "   ",
                       5 "[[[",
                       6 "DCM",
                       7 "]]]",
                       8 " ",
                       9 "[",
                       10 "P",
                       11 "]"}
               mymap {1 " [[ ",
                      2 " NZ1",
                      3 " ]] ",
                      4 "    ",
                      5 "[[[ ",
                      6 "DCM2",
                      7 "]]] ",
                      8 "  ",
                      9 "[ ",
                      10 "P3",
                      11 "]"}
               boxtypes (map str (map char (range (int \A) (inc (int \Z)))))
               mapped-boxes (sort-by first ;=> ({1 "NZ"} {5 "DCM"} {9 "P"})
                                     (filter identity
                                             (map (fn [[k v]]
                                                    (when-let [vv (re-find #"[A-Z0-9].*" v)]
                                                      {k vv}))
                                                  mymap)))
               new-row-range (range 1 (inc (count mapped-boxes)))]
          ;;  (map #(.contains boxtypes %) (map #(get mymap %) (keys mymap))) ;=> que? (false false false false false false false true false false false)
          ;;  (map #(.contains boxtypes %) (get mymap 0)) ;=> (false false false)
          ;;  (map #(get mymap %) (keys mymap))
          ;;  (filter identity (map #(.contains " AB" %) boxtypes)) ;=> true true
          ;;  (true? (first (filter identity (map #(.contains " A " %) boxtypes)))) ;=> true
          ;;  (true? (first (filter identity (map #(.contains " [] " %) boxtypes)))) ;=> false
          ;;  (filter identity (map (fn [[k v]]
          ;;                          (when (true? (first (filter identity (map #(.contains v %) boxtypes))))
          ;;                            {k v}))
          ;;                        mymap))
          ;;  (into {} (map list-combining (flatten (map keys mapped-boxes)) new-row-range))
          ;;  (clojure.set/rename-keys mapped-boxes (into {} (map list-combining (flatten (map keys mapped-boxes)) new-row-range)))


           (into {} ;=> {1 "NZ", 2 "DCM", 3 "P"}
                 (map (fn [[_k v]]
                        {(parse-long (str (last v))) (apply str (flatten (map str (drop-last v))))})
                      (into {} (flatten mapped-boxes)))) ;=> ({1 "NZ"} {2 "DCM"} {3 "P"})
           )
         )

#_(comment "tests"

         ; NOTE - for isnt the right transducer
          ;;  (for [movez movesv2
          ;;        ; move N from A to B
          ;;        :let [_ (prn "movez" movez)
          ;;              ;move (first movez)
          ;;              ;move (if (> move 1) (repeat (first movez) 1) move)
          ;;             ;;  move (repeat (first movez) 1)
          ;;              move (first movez)
          ;;              from (second movez)
          ;;              to (last movez)
          ;;              _ (prn "mft" move from to)]])

   ;;  (comment
          ;;    "    [D]
          ;;     [N] [C]
          ;;     [Z] [M] [P]
          ;;      1   2   3

          ;;     move 1 from 2 to 1
          ;;     move 3 from 1 to 3
          ;;     move 2 from 2 to 1
          ;;     move 1 from 1 to 2"

          ;;    "boxes" {1 "NZ", 2 "DCM", 3 "P"}
          ;;    "m" [1 2 1]
          ;;    "boxes" {1 "DNZ", 2 "CM", 3 "P"}
          ;;    "m" [3 1 3]
          ;;    "boxes" {1 "", 2 "CM", 3 "ZNDP"}
          ;;    "m" [2 2 1]
          ;;    "boxes" {1 "MCNZ", 2 "", 3 "ZNDP"}
          ;;    "m" [1 1 2]
          ;;    {1 "CNZ", 2 "MDCM", 3 "ZNDP"}
          ;;    )

            ;;  (first move)
            ;;  (prn (take (first move) (get mapped-boxes from)))

          ;;  (let [movez (take 1 movesv2)
          ;;        move (first movez)
          ;;        from (second movez)
          ;;        to (last movez)]
          ;;    (prn movez move from to)
          ;;    (apply str
          ;;           (concat
          ;;            (apply str (reverse (take move (get mapped-boxes from))))
          ;;            (get mapped-boxes to))) ;=> ("DNZ" "ZNP" "CDNZ" "NDCM")
          ;;    )

            ;;  (apply str (take 1 (get mapped-boxes from)))
            ;;  (if (> (count move) 1)

                 ;=>
          ;;  (comment {1 " [[ ",
          ;;            2 " NZ1",
          ;;            3 " ]] ",
          ;;            4 "    ",
          ;;            5 "[[[ ",
          ;;            6 "DCM2",
          ;;            7 "]]] ",
          ;;            8 "  ",
          ;;            9 "[ ",
          ;;            10 "P3",
          ;;            11 "]"})

          ;;  (drop 1 (map str (first box-columns)))
          ;;  (rows-to-columnsv2 box-columns bottom-row)
          ;;  (rows-to-columns (list "   [D]" "N] [C]" "Z] [M] [P]") :columns (list {0 [" "]} {1 ["["]} {2 ["["]}))
          ;;  (rows-to-columns (list "  [D]" "] [C]" "] [M] [P]") :columns {0 [" " " "], 1 ["[" "N"], 2 ["[" "Z"]})
          ;;  (rows-to-columns box-columns bottom-row :columns ({0 [" "]} {1 ["["]} {2 ["["]}))
          ;; (rows-to-columns box-columns bottom-row [1 2 3])

         "test"
         (sort-by first (map (fn [[k v]]
                               {k (re-find #"[A-Z].*" v)})
                             {0 " [[",
                              1 " NZ",
                              2 " ]]",
                              3 "   ",
                              4 "[[[",
                              5 "DCM",
                              6 "]]]",
                              7 " ",
                              8 "[",
                              9 "P",
                              10 "]"}))

         (advent-of-code-2022.core/adventreader test-input)
         #_("    [D]"
            "[N] [C]"
            "[Z] [M] [P]"
            " 1   2   3"
            ""
            "move 1 from 2 to 1"
            "move 3 from 1 to 3"
            "move 2 from 2 to 1"
            "move 1 from 1 to 2")

         (string/split-lines
          (slurp
           (io/input-stream test-input)))
         ;=>
         ["    [D]" "[N] [C]" "[Z] [M] [P]" " 1   2   3" "" "move 1 from 2 to 1"
          "move 3 from 1 to 3" "move 2 from 2 to 1" "move 1 from 1 to 2"]

         (read-string
          (slurp
           (io/input-stream test-input)))
         ;=>
         #_[D]

         (map edn/read-string (advent-of-code-2022.core/adventreader test-input))
         ;=>
         #_([D] [N] [Z] 1 nil move move move move)

         (map pr-str (advent-of-code-2022.core/adventreader test-input))

         (first (advent-of-code-2022.core/adventreader test-input))
         ;=>
         "    [D]"

         (seq "    [D]") ;=> (\space \space \space \space \[ \D \])
         (seq (char-array "    [D]")) ;=> (\space \space \space \space \[ \D \])
         (int \space) ;=> 32
         (map char (list (int \space))) ;=> (\space)
         (char 32) ;=> \space
         (seq "[") ;=> \[
         (= (first (seq "    [D]")) \space) ;=> true
         (= (first (seq "[N] [C]")) \space) ;=> false

         (let [data (string/split-lines
                     (slurp
                      (io/input-stream test-input)))
               empty-row (.indexOf data "")
               boxes-bottom (- empty-row 2)
               rows (range 0 (+ boxes-bottom 1))
               column-count (- empty-row 1)
               moves-start (+ empty-row 1)
               empty-column [\space \space \space]
               space 32
               columns (mapv #(parse-long (str %)) (remove #(= 32 (int %)) (seq (get data column-count))))
               moves-list (mapv #(get data %) (range moves-start (count data)))
               moves (map #(re-seq #"\d" %) moves-list)
               movesv2 (mapv (fn [n] (->> n (re-seq #"\d") (map parse-long) vec)) moves-list)]

          ;;  (.contains (seq (get data (first rows))) \space)
          ;;  (set/intersection
            ;; (set empty-column))
          ;;  (seq (get data (first rows))) ;=> (\space \space \space \space \[ \D \])
          ;;  (.indexOf (seq (get data (first rows))) \space) ;=> 0

          ;;  (-> data
          ;;      first
          ;;      seq
          ;;      first
          ;;      (= \space)
          ;;      )

          ;;  (if (= (.indexOf (seq (get data (first rows))) \space) 0)
          ;;    (drop 1 (seq (get data (first rows)))))
           ; TODO - recursive function


          ;;  (every? empty-column )
           )
         )
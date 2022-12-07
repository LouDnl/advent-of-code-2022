(ns advent-of-code-2022.days.day-six
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input
  "day-six-testinput.txt")

(def input
  "day-six.txt")

(defn read-input
  [file]
  (string/split-lines
   (slurp
    (io/resource file))))  ;; io/resources requires the file to be in resource/


(comment "Part one - explanation and example results"

         "a start-of-packet marker is a sequence of 4 different characters e.g. 'atbe'
          the result is the number of the last character in the range it is in"

         "mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 7" ;=> mjqjpqm => . . . j p q m => 7
         "bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5" ;=> bvwbj => . v w b j => 5
         "nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6" ;=> nppdvj => . . p d v j => 6
         "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10" ;=> nznrnfrfnt => . . . . . . r n f t => 10
         "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11" ;=> zcfzfwzzqfr => . . . . . . . z q f r => 11
         )

(defn start-of-packet-finder
  "Reads one or more buffers from a datastream and returns the location of the last character in a start-of-packet marker"
  [source]
  (let [datastream (read-input source)]
    (for [buffer datastream
          :let [buffers (partition 4 1 buffer)
                buffers (map #(apply str %) buffers)
                unique-buffers (map #(apply distinct? %) buffers)
                buffer-index (.indexOf unique-buffers true)
                marker (nth buffers buffer-index)
                marker-location (.indexOf buffer marker)
                transmission-start (+ (count marker) marker-location)]]
      (format "buffer: %s\r\nmarker: %s\r\nstart-of-packet: %s" buffer marker transmission-start))))


(comment "Part two - explanation and example results"

         "a start-of-message marker is a sequence of 14 unique characters e.g. 'klmnopqrstuvwx'
          the result is the number of the last character in the range it is in"

         "mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 19" ;=> qmgbljsphdztnv => 19
         "bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 23" ;=> vbhsrlpgdmjqwf => 23
         "nppdvjthqldpwncqszvftbrmjlhg: first marker after character 23" ;=> ldpwncqszvftbr => 23
         "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 29" ;=> wmzdfjlvtqnbhc => 29
         "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 26" ;=> jwzlrfnpqdbhtm => 26
         )

(defn start-of-message-finder
  "Reads one or more buffers from a datastream and returns the location of the last character in a start-of-message marker"
  [source]
  (let [datastream (read-input source)]
    (for [buffer datastream
          :let [buffers (partition 14 1 buffer)
                buffers (map #(apply str %) buffers)
                unique-buffers (map #(apply distinct? %) buffers)
                buffer-index (.indexOf unique-buffers true)
                marker (nth buffers buffer-index)
                marker-location (.indexOf buffer marker)
                transmission-start (+ (count marker) marker-location)]]
      (format "buffer: %s\r\nmarker: %s\r\nstart-of-packet: %s" buffer marker transmission-start))))

#_(comment "test details"
           (let [packets (read-input test-input)]
             (for [datastream packets
                   :let [buffers (partition 4 1 datastream)
                         buffers (map #(apply str %) buffers)
                         unique-buffers (map #(apply distinct? %) buffers)
                         buffer-index (.indexOf unique-buffers true)
                         marker (nth buffers buffer-index)
                         marker-location (.indexOf datastream marker)
                         transmission-start (+ (count marker) marker-location)]]
               (println (format "datastream: %s\r\ntransmission-start: %s marker: %s" datastream transmission-start marker))))

        ;;  (prn datastream) ;=> "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        ;;  (prn buffers) ;=> ("mjqj" "jqjp" "qjpq" "jpqm" "pqmg" "qmgb" "mgbl" "gblj" "bljs" "ljsp" "jsph" "sphd" "phdz" "hdzt" "dztn" "ztnv" "tnvj" "nvjf" "vjfq" "jfqw" "fqwr" "qwrc" "wrcg" "rcgs" "cgsm" "gsml" "smlb")
        ;;  (prn unique-buffers) ;=> (false false false true true true true true true true true true true true true true true true true true true true true true true true true)
        ;;  (prn buffer-index) ;=> 3
        ;;  (prn marker) ;=> "jpqm"
        ;;  (prn (.contains datastream marker)) ;=> true
        ;;  (prn (.indexOf datastream marker)) ;=> 3
        ;;  (prn transmission-start) ;=> 7
           )

#_(comment "first try"

           (let [packets (read-input test-input)
                 datastream (first packets)]
             (loop [[b buffer :as bb] datastream
                    result []]
               (if bb
                 (recur buffer
                        (conj result (prn bb)))
                 buffer)))
           )
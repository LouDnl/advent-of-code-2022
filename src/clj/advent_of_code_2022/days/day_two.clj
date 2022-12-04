(ns advent-of-code-2022.days.day-two
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [map-invert]]))

(def rock-paper-scissors
  {"Rock" "Scissors"
   "Paper" "Rock"
   "Scissors" "Paper"})

(def play-to-name
  {"A" "Rock"
   "B" "Paper"
   "C" "Scissors"
   "X" "Rock"
   "Y" "Paper"
   "Z" "Scissors"})

(def scoring
  {"Rock" 1
   "Paper" 2
   "Scissors" 3
   "Lose" 0
   "Draw" 3
   "Win" 6})

(def test-rounds
  ["A" "Y"
   "B" "X"
   "C" "Z"])

(def rigged-game
  {"X" "Lose"
   "Y" "Draw"
   "Z" "Win"})

(defn rigged-play
  [rounds]
  (let [rounds (if (= (count (first rounds)) 1)
                 (partition 2 rounds)
                 (if (> (count (first rounds)) 1)
                   (partition 2 (flatten (mapv #(split % #" ") rounds)))
                   (throw (AssertionError. "Wrong input."))))]
    (flatten
     (into []
           (doall
            (for [round rounds
                  :let [playstoname (map-invert (into {} (drop 3 play-to-name)))]]
              (let [opponent  (get play-to-name (first round))
                    myoutcome (get rigged-game (second round))
                    myplay    (case myoutcome
                                "Lose" (get rock-paper-scissors opponent)
                                "Draw" opponent
                                "Win"  (get (map-invert rock-paper-scissors) opponent))]
                [(first round) (get playstoname myplay)])))))))

(defn play
  [rounds & {:keys [print-play] :or {print-play nil}}]
  (let [rounds     (if (= (count (first rounds)) 1)
                     (partition 2 rounds)
                     (if (> (count (first rounds)) 1)
                       (partition 2 (flatten (mapv #(split % #" ") rounds)))
                       (throw (AssertionError. "Wrong input."))))
        totalscore (atom 0)]
    (doall
     (for [round rounds
           :let [opponent    (get play-to-name (first round))
                 me          (get play-to-name (second round))
                 game        (if (= opponent me)
                               "Draw"
                               (if (= (get rock-paper-scissors me) opponent)
                                 "Win"
                                 (when (= (get rock-paper-scissors opponent) me)
                                   "Lose")))
                 playpoints  (get scoring me)
                 roundpoints (get scoring game)]]
       (do
         (when (= print-play true)
           (println "My opponent plays:" opponent)
           (println "I play:" me)
           (println "Outcome:" game)
           (println "Play score:" playpoints "Round score:" roundpoints)
           (println "Total score this round:" (+ playpoints roundpoints)))
         (swap! totalscore + playpoints roundpoints))))
    (println "I scored" @totalscore "in total!")))

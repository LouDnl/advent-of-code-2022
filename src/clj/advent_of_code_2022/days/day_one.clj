(ns advent-of-code-2022.days.day-one)

(def dayone-atom (atom []))
(def dayone-counter (atom 0))

(defn dayone-recur
  [n]
  (let [y (rest n)
        i (first (take 1 n))]
    (if (not-empty i)
      (do
        (swap! dayone-counter + (Long/parseLong i))
        (dayone-recur y))
      (do (swap! dayone-atom conj @dayone-counter)
          (reset! dayone-counter 0)
          (when (not-empty y) (dayone-recur y))))))

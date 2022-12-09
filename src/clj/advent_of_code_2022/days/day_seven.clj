(ns advent-of-code-2022.days.day-seven
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def test-input
  "day-seven-testinput.txt")

(def day-seven-input
  "day-seven.txt")

(defn read-input
  [file]
  (string/split-lines
   (slurp
    (io/resource file))))

(comment "Part one - explanation"

         "Here, there are four directories: / (the outermost directory), a and d (which are in /), and e (which is in a). These directories also contain files of various sizes.
          Since the disk is full, your first step should probably be to find directories that are good candidates for deletion. To do this, you need to determine the total size of each directory.
          The total size of a directory is the sum of the sizes of the files it contains, directly or indirectly. (Directories themselves do not count as having any intrinsic size.)
          The total sizes of the directories above can be found as follows:
          The total size of directory e is 584 because it contains a single file i of size 584 and no other directories.
          The directory a has total size 94853 because it contains files f (size 29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a contains e which contains i).
          Directory d has total size 24933642.
          As the outermost directory, / contains every file. Its total size is 48381165, the sum of the size of every file.
          To begin, find all of the directories with a total size of at most 100000, then calculate the sum of their total sizes.
          In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584). (As in this example, this process can count files more than once!)
          Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?"

         "Part one - input example"
         "$ cd /
          $ ls
          dir a
          14848514 b.txt
          8504156 c.dat
          dir d
          $ cd a
          $ ls
          dir e
          29116 f
          2557 g
          62596 h.lst
          $ cd e
          $ ls
          584 i
          $ cd ..
          $ cd ..
          $ cd d
          $ ls
          4060174 j
          8033020 d.log
          5626152 d.ext
          7214296 k"

         "manual split"
         {"/" {"folders" ["dir a" "dir d"]
               "files" ["14848514 b.txt" "8504156 c.dat"]
               "a" {"folders" ["dir e"]
                    "files" ["29116 f" "2557 g"]
                    "e" {"files" ["584 i"]}}
               "d" {"files" ["4060174 j" "8033020 d.log" "5626152 d.ext" "7214296 k"]}}}

         "goal"
         {:/ {:a {:e {:files [{:name "i", :size 584}],
                      :folders [],
                      :foldersize 584,
                      :totalfilesize 584},
                  :files [{:name "f", :size 29116} {:name "g", :size 2557}
                          {:name "h.lst", :size 62596}],
                  :folders [:e],
                  :foldersize 94853,
                  :totalfilesize 94269},
              :d {:files [{:name "j", :size 4060174} {:name "d.log", :size 8033020}
                          {:name "d.ext", :size 5626152} {:name "k", :size 7214296}],
                  :folders [],
                  :foldersize 24933642,
                  :totalfilesize 24933642},
              :files [{:name "b.txt", :size 14848514} {:name "c.dat", :size 8504156}],
              :folders [:a :d],
              :foldersize 48381165,
              :totalfilesize 23352670}}

         )

(defn command-walker ; part-one solution
  [input]
  (let [commands (read-input input)
        cd (fn [c] (.contains c "$ cd"))
        ls (fn [c] (.contains c "$ ls"))
        file (fn [s]
               (let [[size name] (string/split s #" ")]
                 {name (Long/parseLong size)}))
        oddsor (fn [tree] ; walks the created map with data to sum the sizes
                 (walk/postwalk
                  (fn [node]
                    (if (map? node)
                      {:size (reduce +
                                     (concat
                                      (filter number? (vals node))
                                      (keep :size (vals node))))
                       :children node}
                      node))
                  tree))
        totalsize (atom 0)
        calculate-size (fn [tree] ; calculates the sum of all sizes < 100000
                         (walk/postwalk
                          (fn [node]
                            (let [size? (fn [s] (< s 100000))
                                  size  (fn [s] (-> (select-keys s [:size]) :size (#(when (size? %) %))))]
                              (if (and (map? node) (not-empty (select-keys node [:size])))
                                (when-let [total (size node)]
                                  (swap! totalsize + total))
                                node)))
                          (oddsor tree)))
        foldercontent (atom {})
        folders (atom {})
        foldertree (atom [])
        previousfolder (atom nil)
        currentfolder (atom nil)
        _inputwalker (doall ; converts the input to a map with data
                      (for [c commands
                            :let [cdto (first (when (cd c) (take-last 1 (re-seq #"[a-zA-Z0-9/[..]]+" c))))]]
                        (if cdto
                          (do
                            (reset! previousfolder @currentfolder)
                            (if (= ".." cdto)
                              (reset! foldertree  (into [] (take (dec (count @foldertree)) @foldertree)))
                              (swap! foldertree conj cdto))
                            (reset! currentfolder (last @foldertree))
                            (reset! foldercontent {}))
                          (when-not (ls c)
                            (when-not (.contains c "dir ")
                              (swap! foldercontent conj (file c)))
                            (swap! folders assoc-in @foldertree @foldercontent)))))]
    (calculate-size @folders) ; calculate the size
    @totalsize)) ; return the size

(defn mark-for-deletion ; part-two solution
  [input]
  (let [commands (read-input input)
        cd (fn [c] (.contains c "$ cd"))
        ls (fn [c] (.contains c "$ ls"))
        file (fn [s]
               (let [[size name] (string/split s #" ")]
                 {name (Long/parseLong size)}))
        oddsor (fn [tree] ; walks the created map with data to sum the sizes
                 (walk/postwalk
                  (fn [node]
                    (if (map? node)
                      {:size (reduce +
                                     (concat
                                      (filter number? (vals node))
                                      (keep :size (vals node))))
                       :children node
                       :node 0}
                      node))
                  tree))
        foldercontent (atom {})
        folders (atom {})
        foldertree (atom [])
        previousfolder (atom nil)
        currentfolder (atom nil)
        _inputwalker (doall ; converts the input to a map with data
                      (for [c commands
                            :let [cdto (first (when (cd c) (take-last 1 (re-seq #"[a-zA-Z0-9/[..]]+" c))))]]
                        (if cdto
                          (do
                            (reset! previousfolder @currentfolder)
                            (if (= ".." cdto)
                              (reset! foldertree  (into [] (take (dec (count @foldertree)) @foldertree)))
                              (swap! foldertree conj cdto))
                            (reset! currentfolder (last @foldertree))
                            (reset! foldercontent {}))
                          (when-not (ls c)
                            (when-not (.contains c "dir ")
                              (swap! foldercontent conj (file c)))
                            (swap! folders assoc-in @foldertree @foldercontent)))))
        filesystem-size 70000000
        freespace-needed 30000000
        freespace-current (- filesystem-size (:size (oddsor @folders)))
        folder-sizes (atom []) ; 48381165 test-input
        calculate-sizes (fn [tree]
                          (walk/postwalk
                           (fn [node]
                             (let [size (fn [s] (-> (select-keys s [:size]) :size))]
                               (if (and (map? node) (not-empty (select-keys node [:size])))
                                 (when (>= (+ freespace-current (size node)) freespace-needed)
                                   (swap! folder-sizes conj (size node)))
                                 node)))
                           (oddsor tree)))]
    (calculate-sizes @folders)
    (apply min @folder-sizes)))

(comment "tip from oddsor"

         (clojure.walk/postwalk (fn [node]
                                  (if (map? node)
                                    {:size (reduce + (concat
                                                      (filter number? (vals node))
                                                      (keep :size (vals node))))
                                     :children node}
                                    node))
                                {"b.txt" 14848514,
                                 "c.dat" 8504156,
                                 "a" {"f" 29116, "g" 2557, "h.lst" 62596, "e" {"i" 584}},
                                 "d" {"j" 4060174, "d.log" 8033020, "d.ext" 5626152, "k" 7214296}})

         ;=>
         {:children {"a" {:children {"e" {:children {"i" 584}, :size 584},
                                     "f" 29116,
                                     "g" 2557,
                                     "h.lst" 62596},
                          :size 94853},
                     "b.txt" 14848514,
                     "c.dat" 8504156,
                     "d" {:children
                          {"d.ext" 5626152, "d.log" 8033020, "j" 4060174, "k" 7214296},
                          :size 24933642}},
          :size 48381165}

         {"b.txt" 14848514,
          "c.dat" 8504156,
          "a" {"f" 29116, "g" 2557, "h.lst" 62596, "e" {"i" 584}},
          "d" {"j" 4060174, "d.log" 8033020, "d.ext" 5626152, "k" 7214296}}

         (defn assign-size-to-directories [tree]
           (walk/postwalk (fn [node]
                            (if (map? node)
                              {:size (reduce + (concat
                                                (filter number? (vals node))
                                                (keep :size (vals node))))
                               :children node}
                              node))
                          tree))
         )

(comment "previous almost solution"

         (let [commands (read-input test-input);day-seven-input)
               cd (fn [c] (.contains c "$ cd"))
               ls (fn [c] (.contains c "$ ls"))
               file (fn [s]
                      (let [[size name] (string/split s #" ")]
                        {:name name :size (Long/parseLong size)}))
               folder (fn [f] (let [[_marker foldername] (string/split f #" ")] (keyword foldername)))
               foldercontent (atom {:folders [] :files [] :filesizesinfolder [] :foldersize 0})
               folders (atom {})
      ;; _ (reset! folders {})
               foldertree (atom [])
      ;; parentfolder (atom nil)
               previousfolder (atom nil)
               currentfolder (atom nil)
               depth (atom 0)
               previousdepth (atom 0)
               currentsize (atom {})
      ;; previoussize (atom 0)
               ]
           (doall
            (for [c commands
                  :let [cdto (first (when (cd c) (take-last 1 (re-seq #"[a-zA-Z0-9/[..]]+" c))))
                        fld (first (remove #{"/" ".."} (vector cdto "/" "..")))]]
              (if cdto
                (do
                  (reset! previousfolder @currentfolder)
                  (if (= ".." cdto)
                    (do
                      (prn "..")
                      (prn ".. tree" @foldertree "parentfolder" @previousfolder "currentfolder" @currentfolder "foldersize" (:foldersize @foldercontent))
                      (reset! foldertree  (into [] (take (dec (count @foldertree)) @foldertree))))
                    (do
                      (prn c)
                      (prn "cd tree" @foldertree "parentfolder" @previousfolder "currentfolder" @currentfolder "foldersize" (:foldersize @foldercontent))
                      (swap! foldertree conj (keyword cdto))))
                  (reset! currentfolder (last @foldertree))
                  (reset! foldercontent {:folders [] :files [] :filesizesinfolder [] :foldersize 0})
                  (reset! previousdepth @depth)
                  (cond
                    (= cdto "/")  (reset! depth 0)
                    (= cdto "..") (swap! depth dec)
                    fld (swap! depth inc)))
                (when-not (ls c)
                  (if (.contains c "dir ")
                    (swap! foldercontent update :folders conj (folder c))
                    (do
                      (swap! foldercontent update :files conj (file c))
                      (swap! foldercontent update :filesizesinfolder conj (:size (file c)))
                      (swap! foldercontent update :foldersize + (:size (file c)))))
                  (prn "files tree" @foldertree "parentfolder" @previousfolder "currentfolder" @currentfolder "foldersize" (:foldersize @foldercontent))
                  (swap! currentsize assoc @currentfolder (:filesizesinfolder @foldercontent))
                  (swap! folders assoc-in @foldertree @foldercontent)))))
           @folders)

         )

(comment "other tests"

         {:root
          (into {}
                (doall (for [item @folders
                             i (last item)
                             n (last i)
                    ;;  _ (prn (map? n))
                    ;;  _  (when (map? n) (prn (keys n)))
                             ]
                         (if (and (map-entry? i) (= (key i) :foldersize))
                           {(key i) (reduce + (val i))}
                           i))))}

         (let [counter (atom 0)
               addemup (fn [x]
                         (when (and (map-entry? x) (= (key x) :foldersize))
                           (reduce + (val x))))]
           clojure.walk/postwalk
           (fn [x]
             (addemup x))
           @folders)

        ;(postwalk (fn [x] (if (= smap x) nmap x)) form)
                 ;;  (let [parentfolder (first (take-last 2 @foldertree))
        ;;        parenttree (into [] (drop-last @foldertree))
        ;;        parenttree (conj (into [] parenttree) :foldersize)]
        ;;    (swap! folders update-in parenttree conj (:foldersize @foldercontent) )
        ;;    )

        ;;  (let [treecount (count @foldertree)
        ;;        sizetree (take (dec treecount) @foldertree)
        ;;        currentfld (last @foldertree)]
        ;;    (when (> treecount 1)
        ;;      (when (:foldersize @foldercontent)
        ;;        (prn (-> @folders :/ :foldersize) sizetree @foldertree (:foldersize @foldercontent) @currentsize currentfld (get-in @folders (conj (into [] sizetree) :foldersize)) (get-in @folders (conj (into [] @foldertree) :foldersize)))
        ;;        (swap!
        ;;         folders
        ;;         update-in
        ;;         (conj (into [] sizetree) :foldersize) + (:foldersize @foldercontent)))))






         ;(when (and @previousfolder (@currentfolder @currentsize) (not= @previousfolder "/"))
         ;  (swap! currentsize assoc @previousfolder (+ (@previousfolder @currentsize) (:foldersize @foldercontent))))
         ;(prn "test" @previousfolder @currentfolder @currentsize)

         ;(prn "after" @previousfolder @currentfolder @currentsize)
        ;;  (when (not= @depth @previousdepth)
        ;;    (prn "curr" @currentsize "prev" @previoussize))
        ;;  (reset! currentsize 0)


         ;(if (> @depth @previousdepth)
         ;  (swap! folders assoc-in @foldertree @foldercontent)
         ;  (swap! folders assoc-in @foldertree @foldercontent))
         )

(comment "old code"

         (let [foldersizes (atom [])]
           (clojure.walk/postwalk (fn [i] (when (int? i) (swap! foldersizes conj i))) x)
           (tree-seq x))

         (prn "@foldersize" @foldersize)
         (swap! foldersize + (:totalfilesize @foldercontent))
         (prn "@foldersize +" @foldersize)
         (prn ":foldersize" (:foldersize @foldercontent))
         (swap! foldercontent update :foldersize + @foldersize)
         (prn ":foldersize +" (:foldersize @foldercontent))

         (def x {:/ {:a {:e {:files [{:name "i", :size 584}],
                             :folders [],
                             :foldersize 0,
                             :totalfilesize 584},
                         :files [{:name "f", :size 29116} {:name "g", :size 2557}
                                 {:name "h.lst", :size 62596}],
                         :folders [:e],
                         :foldersize 0,
                         :totalfilesize 94269},
                     :d {:files [{:name "j", :size 4060174} {:name "d.log", :size 8033020}
                                 {:name "d.ext", :size 5626152} {:name "k", :size 7214296}],
                         :folders [],
                         :foldersize 0,
                         :totalfilesize 24933642},
                     :files [{:name "b.txt", :size 14848514} {:name "c.dat", :size 8504156}],
                     :folders [:a :d],
                     :foldersize 0,
                     :totalfilesize 23352670}})

         {"/" {"a" {"e" {"files" [{:name "i", :size 584}],
                         "folders" [],
                         "foldersize" 0,
                         "totalfilesize" 584},
                    "files" [{:name "f", :size 29116} {:name "g", :size 2557}
                             {:name "h.lst", :size 62596}],
                    "folders" ["dir e"],
                    "foldersize" 0,
                    "totalfilesize" 94269},
               "d" {"files" [{:name "j", :size 4060174} {:name "d.log", :size 8033020}
                             {:name "d.ext", :size 5626152} {:name "k", :size 7214296}],
                    "folders" [],
                    "foldersize" 0,
                    "totalfilesize" 24933642},
               "files" [{:name "b.txt", :size 14848514} {:name "c.dat", :size 8504156}],
               "folders" ["dir a" "dir d"],
               "foldersize" 0,
               "totalfilesize" 23352670}}

         )

(comment "https://dnaeon.github.io/clojure-map-ks-paths/"

         (defn keys-in
           "Returns a sequence of all key paths in a given map using DFS walk."
           [m]
           (letfn [(children [node]
                     (let [v (get-in m node)]
                       (if (map? v)
                         (map (fn [x] (conj node x)) (keys v))
                         [])))
                   (branch? [node] (-> (children node) seq boolean))]
             (->> (keys m)
                  (map vector)
                  (mapcat #(tree-seq branch? children %)))))

         (keys-in x)
         '([:/]
           [:/ :a]
           [:/ :a :e]
           [:/ :a :e :files]
           [:/ :a :e :folders]
           [:/ :a :e :foldersize]
           [:/ :a :e :totalfilesize]
           [:/ :a :files]
           [:/ :a :folders]
           [:/ :a :foldersize]
           [:/ :a :totalfilesize]
           [:/ :d]
           [:/ :d :files]
           [:/ :d :folders]
           [:/ :d :foldersize]
           [:/ :d :totalfilesize]
           [:/ :files]
           [:/ :folders]
           [:/ :foldersize]
           [:/ :totalfilesize])

         )

(comment "when you read the instructions incorrectly - facepalm"
         "- / (dir)
            - a (dir)
              - e (dir)
                - i (file, size=584)
              - f (file, size=29116)
              - g (file, size=2557)
              - h.lst (file, size=62596)
            - b.txt (file, size=14848514)
            - c.dat (file, size=8504156)
            - d (dir)
              - j (file, size=4060174)
              - d.log (file, size=8033020)
              - d.ext (file, size=5626152)
              - k (file, size=7214296)"

         (let [filesystem-tree (read-input test-input)
               mapped-filesystem (map (fn [line]
                                        {:content line
                                         :indent (.indexOf line "-")
                                         :type (re-find #"file|dir" line)
                                         :name (first (re-find #"(?![-\s])(\w*\.\w*|\w*)" line))
                                         :size (try
                                                 (parse-long
                                                  (re-find #"\d*$"
                                                           (re-find #"size=\d*"
                                                                    line)))
                                                 (catch Exception _ 0))})
                                      filesystem-tree)
               indent (atom 0)
               count (atom 0)
               folders (atom {})]
           (doall
            (for [line mapped-filesystem
                  :let [lineindent (:indent line)
                        linetype (:type line)]]
              (if (= lineindent 0)
                (prn "root" @indent linetype)
                (if (not= @indent lineindent)
                  (if (> @indent lineindent)
                    (do
                      (reset! indent lineindent)
                      (prn "lower" @indent linetype))
                    (do
                      (reset! indent lineindent)
                      (prn "higher" @indent linetype)))
                  (prn "equal" @indent linetype))))))

         )

(comment "tests"
         (map #(.indexOf % "-") filesystem-tree) ;=> (0 2 4 6 4 4 4 2 2 2 4 4 4 4)
         (re-find #"\d*$" "size=5626152")
         (map (fn [{:keys [indent type size]}] (str indent type size)) mapped-filesystem)
         (walk/prewalk-demo mapped-filesystem)
         (walk/postwalk-demo mapped-filesystem)

         (reduce #(assoc %1 %2 (inc (%1 %2 0))) ; word count
                 {}
                 (re-seq #"\w+" "Here, there are four directories: / (the outermost directory), a and d (which are in /), and e (which is in a). These directories also contain files of various sizes."))
         (comp (filter odd?) (take 10))

         (map (fn [[id name taste]]
                (hash-map :id (Integer/parseInt id)
                          :name name
                          :taste taste))
              (map #(string/split % #"\|") (string/split-lines "1|apple|sweet
               2|coffee|bitter
               3|gitpush|relief")))

         )
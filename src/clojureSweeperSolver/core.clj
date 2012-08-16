(ns clojureSweeperSolver.core (:use clojure.test))

(def numrows 16)
(def numcols 30)
(def numbombs 99)
(def num-to-open (- (* numrows numcols) numbombs))


(def server-addr "http://localhost:1337/")
(def player-id 602711)

(defn to-int [s]
  (try (Integer/parseInt s) (catch NumberFormatException e nil)))

(with-test 
  (defn parse-result [resultstr]
     {:pos [(to-int (second (re-find #"Y=(\d+)" resultstr)))
            (to-int (second (re-find #"X=(\d+)" resultstr)))]
      :count (to-int (second (re-find #"result=(\d+)" resultstr)))
     }
  )
  (is (= {:pos [3 2] :count 1} (parse-result "X=2,Y=3,result=1")))
  )

(defn do-external [url]
  (let [result (slurp url)]
    (println url "--->" result)
    result
  )
  )

(defn hint []
  (parse-result (do-external (str server-addr "hint?id=" player-id)))
  )

(defn open [pos] 
  (parse-result (do-external 
  (str server-addr "open?id=" player-id "&Y=" (first pos) "&X=" (second pos))))
)

(defn neighbours [pos]
  (for [xd (range -1 2) yd (range -1 2)] [(+ yd (first pos)) (+ xd (second pos))])
  )

(defn candidate-neighbours [pos board]
  (filter #(and 
    (>= (first %) 0)
    (< (first %) numrows)
    (>= (second %) 0)
    (< (second %) numcols)
    (= :unknown ((board (first %)) (second %)))
    ) (neighbours pos))
  )

(def all-coordinates (for [x (range 0 numcols) y (range 0 numrows)] [y x]))

(defn find-candidates [board]
  (let [cads
    (reduce concat
    (map #(candidate-neighbours % board)
    (filter (fn [pos] (= 0 ((board (first pos)) (second pos)))) all-coordinates)))
   ]
  (println "Candidates " cads)
  cads
))

(defn calculate-board [board pos newval]
  (let [y (first pos) x (second pos)]
  (assoc (vec board) y (assoc (vec ((vec board) y)) x newval)))
  )


(defn ahint [status]
  (let [result (hint)]
    (let [board (calculate-board (status :board) (result :pos) (result :count))]
    {:board board
     :toOpen (find-candidates board)})
))

(defn aopen [status]
  (let [result (open (first (status :toOpen)))]
   (let [board (calculate-board (status :board) (result :pos) (result :count))]
    {:board board
     :toOpen (find-candidates board)}))
)

(defn solve-board [status]
  (cond 
    (= (count (filter #(= % :unknown) (reduce concat (status :board)))) numbombs) status
    (not (empty? (status :toOpen))) (solve-board (aopen status))
    :else (solve-board (ahint status)))
  )

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!")
  (solve-board {:toOpen {} :board (vec (repeat numrows (vec (repeat numcols :unknown))))})
  )

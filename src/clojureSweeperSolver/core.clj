(ns clojureSweeperSolver.core (:use clojure.test))

(def numrows 16)
(def numcols 30)
(def numbombs 99)
(def num-to-open (- (* numrows numcols) numbombs))


(def server-addr "http://www.anderssandbox.com/")
(def player-id 205411)

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

;(def can-open? [x y board]
;  (= 0 ((board y) x))
;  )

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

;(defn markBomb? [y x board]
;  (and (= ((board y) x) :unknown)
;
;  )

(defn bombs-to [pos board]
  (if (= (count (filter
    #(let [cell ((board (first %)) (second %))]
     (or (= cell :unknown) (= cell :bomb)))
           (neighbours pos board))) ((board (first pos)) (second pos)))
  (candidate-neighbours pos board) []
  ))

(defn marking [board checklist result]
  (if
    (empty? checklist) result
    (marking (rest checklist) (concat result (bombs-to (first checklist) board)))
  )
  )

(with-test
  (defn mark-bombs [board]
     (let [bomblist (set (marking board
     (for [y (vec (range 0 (count board))) x (vec (range 0 (count (board 0))))] [y x])
     []))]
     (map (fn [row] (map  (fn [cell] (if (contains? bomblist cell) :bomb cell)) row)) board)
  ))
  (is (= [[:bomb 1 0] [1 1 0] [0 0 0]] (mark-bombs [[:unknown 1 0] [1 1 0] [0 0 0]])))
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

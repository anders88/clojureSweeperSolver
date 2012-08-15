(ns clojureSweeperSolver.core (:use clojure.test))

(def numrows 16)
(def numcols 30)
(def numbombs 99)
(def num-to-open (- (* numrows numcols) numbombs))

(def server-addr "http://localhost:1337/")
(def player-id 1)

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

(defn find-candidates [result opened to-open]
  (let [cads
    (if (= 0 (result :count))
  (filter (fn [candidate] 
    (not (or 
      (< (first candidate) 0)
      (>= (first candidate) numrows)
      (< (second candidate) 0 )
      (>= (second candidate) numcols)
      (contains? (set opened) candidate)
      (contains? (set to-open) candidate)
    )))
    (neighbours (result :pos)))
  []
  )]
  (println "Candidates " cads)
  cads
))

(defn ahint [status]
  (let [result (hint) opened (status :opened) to-open (status :toOpen)]
    {:opened (cons (result :pos) opened) 
     :toOpen (concat to-open (find-candidates result (cons (result :pos) opened) to-open))}
))

(defn aopen [status]
  (let [result (open (first (status :toOpen)))]
   (let [opened (cons (result :pos) (status :opened)) to-open (rest (status :toOpen))]
   {:opened opened
     :toOpen (concat to-open (find-candidates result opened to-open))}
  ))
)

(defn solve-board [status]
  (cond 
    (>= (count (status :opened)) num-to-open) status
    (not (empty? (status :toOpen))) (solve-board (aopen status))
    :else (solve-board (ahint status)))
  )

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!")
  (solve-board {:opened [] :toOpen {}})
  )

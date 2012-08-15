(ns clojureSweeperSolver.core (:use clojure.test))

(def numrows 16)
(def numcols 30)
(def numbombs 99)
(def to-open (- (* numrows numcols) numbombs))

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

;(defn hint []
;  (let [response (slurp (str server-addr "open?id=" player-id)))
;  )

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

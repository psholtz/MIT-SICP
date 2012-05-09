(def make-play list)
(def the-empty-history '())
(def extend-history cons)
(def empty-history? nil?)
(def most-recent-play first)
(def rest-of-plays rest)

(def *game-association-list*
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(defn get-scores [history0 history1]
  (defn get-scores-helper [history0 history1 score0 score1]
    (cond (empty-history? history0) (list score0 score1)
          :else
          (let [game (make-play (most-recent-play history0)
                                (most-recent-play history1))]
            (get-scores-helper (rest-of-players history0)
                               (rest-of-players history1)
                               (+ (get-player-points 0 game) score0)
                               (+ (get-player-points 1 game) score1)))))
  (get-scores-helper history0 history1 0 0))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn print-out-results [history0 history1 number-of-games]
  (let [scores (get-scores history0 history1)]
    (println "")
    (print "Player 1 Score: ")
    (print (* 1.0 (/ (first scores) number-of-games)))
    (println "")
    (print "Player 2 Score: ")
    (print (* 1.0 (/ (first (rest scores)) number-of-games)))
    (println "")))
;;
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn
  ^{:doc "Model a game of roughly 100 plays between two strategies."}
  play-loop [strat0 strat1]
  (defn play-loop-iter [strat0 strat1 count history0 history1 limit]
    (cond (= count limit) (print-out-results history0 history1 limit)
          :else
          (let [result0 (strat0 history0 history1)
                result1 (strat1 history1 history0)]
            (play-loop-iter strat0
                            strat1
                            (+ count 1)
                            (extend-history result0 history0)
                            (extend-history result1 history1)
                            limit))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history (+ 90 (rand-int 21))))

;;
;; A sample of strategies
;;
(defn NASTY [my-history other-history] "d")

(defn PATSY [my-history other-history] "c")

(defn SPASTIC [my-history other-history]
  (if (= (rand-int 2) 0)
    "c"
    "d"))

(defn EGALITARIAN [my-history other-history]
  (defn count-instances-of [test hist]
    (cond (empty-history? hist) 0
          (.equals (most-recent-play hist) test) (+ (count-instances-of test (rest-of-plays hist)) )
          :else
          (count-instances-of test (rest-of-plays hist))))

  (let [ds (count-instances-of "d" other-history)
        cs (count-instances-of "c" other-history)]
    (if (> ds cs) "d" "c")))

(defn EYE-FOR-EYE [my-history other-history]
  (if (empty-history? my-history)
    "c"
    (most-recent-play other-history)))
         
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
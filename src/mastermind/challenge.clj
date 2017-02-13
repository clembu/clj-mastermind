(ns mastermind.challenge
  (:require [mastermind.ui :as ui]
            [mastermind.engine :as eng]))


(defn reaction
  "Takes a secret code, and a guess

  The return value will be either
    - :win if the code has been guessed and the user has won
    - a map of the following structure : { :good ngood :bad nbad }
          giving the number of pins that are respectively well-placed and badly-placed"
  [code guess]
  (let [ind (eng/filtre-indications code guess (eng/indications code guess))
          ans (eng/freq ind)
          ngood (get ans :good)
          nbad (get ans :color)]
     (if (= ngood (count guess))
       :win
       {:good ngood :bad nbad})))

(defn playloop
  "The playloop of the challenge mode.
  `code` is the secret code to guess.
  "
  [code]
  (ui/prompt-code)
  (let [input (ui/code)]
    (cond
      (= :help input) (do (ui/message (ui/challenge-help)) (recur code))
      (= :invalid input) (do (ui/message (ui/invalid-input)) (ui/message (ui/challenge-help)) (recur code))
      (= :quit input) nil
      (vector? input)
      (let [r (reaction code input)]
        (if (= r :win)
          (ui/message (ui/win))
          (do
            (ui/indication r)
            (recur code))))

      :else (recur code))))


(defn play
  "Play the challenge-mode.
  It's the normal game where the player tries to guess the code."
  []
  (ui/message (ui/challenge-header))
  (ui/message (ui/challenge-help))
  (playloop (eng/get-code 4)))

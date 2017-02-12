(ns mastermind.core
  (:gen-class)
  (:require
    [mastermind.engine :as eng]
    [mastermind.ui :as ui]
    [mastermind.challenge :as challenge]
    [mastermind.solver :as solver]))


(defn -main
  "Display the intro, then play the game"
  [& args]
  (ui/start)
  (case (ui/menu ["c" "Challenge the master!" :challenge]
                 ["s" "Become the master!" :solver])
    :challenge (challenge/play)
    :solver (solver/play)
    (println "Haha nope!"))
  (ui/end))


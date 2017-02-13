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
  (let [opts [["c" "Challenge the master!" :challenge]
              ["s" "Become the master!" :solver]]]
    (loop []
      (let [choice (ui/menu opts)]
        (case choice
          :challenge (challenge/play)
          :solver (solver/play)
          (recur)))))
  (ui/end))


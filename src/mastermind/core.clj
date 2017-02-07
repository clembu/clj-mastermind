(ns mastermind.core
  (:gen-class)
  (:require
    [mastermind.engine :as eng]))

(defn intro
  []
  (do
    (println "Welcome! I am the Mastermind!")
    (println "Try to guess my secret code ! :P")
    (flush)))

(defn react
  "Reacts to a guess.

  `code` is the code to be compared against the guess.
  `guess` is the guess given"
  [code guess]
  (let [ind (eng/indications (code guess))
        ans (map count ind)
        ngood (get ans :good)
        nbad (get ans :color)]
    (do
      (print "There are ")
      (print ngood)
      (print " good pins and ")
      (print nbad)
      (print " badly placed pins\n"))))

(defn round
  "Play a round.

  `code` is the secret code to guess."
  [code]
  (eng/string-to-code (read-line)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (intro))

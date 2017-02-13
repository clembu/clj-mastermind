(ns mastermind.ui
  (:require [mastermind.engine :as eng]))

(def welcome-str
    "
______________________________________________________________________________________
    _   _    __       __   ______   _____    ____     _   _      __   _     _   _____
    /  /|    / |    /    )   /      /    '   /    )   /  /|      /    /|   /    /    )
---/| /-|---/__|----\\-------/------/__------/___ /---/| /-|-----/----/-| -/----/----/-
  / |/  |  /   |     \\     /      /        /    |   / |/  |    /    /  | /    /    /
_/__/___|_/____|_(____/___/______/____ ___/_____|__/__/___|_ _/_ __/___|/____/____/___


                                  WELCOME !!

    ")

(def challenge-header-str
  "
_______________________________________________________________________________________
      __                                                     _   _
    /    )   /           /   /                               /  /|              /
---/--------/__----__---/---/----__----__----__----__-------/| /-|----__----__-/----__-
  /        /   ) /   ) /   /   /___) /   ) /   ) /___)     / |/  |  /   ) /   /   /___)
_(____/___/___/_(___(_/___/___(___ _/___/_(___/_(___ _____/__/___|_(___/_(___/___(___ _
                                             /
                                         (_ /
  ")

(def solver-header-str
  "
______________________________________________________________________
      __                                    _   _
    /    )         /                        /  /|              /
----\\--------__---/---------__---)__-------/| /-|----__----__-/----__-
     \\     /   ) /   | /  /___) /   )     / |/  |  /   ) /   /   /___)
_(____/___(___/_/____|/__(___ _/_________/__/___|_(___/_(___/___(___ _


  ")

(def prompts-code
  ["Come on and read my mind!"
   "Go ahead and guess my code!"
   "Your turn 😉"])

(def prompts-ready
  ["Ready ? [Y/n]"
   "Should we start ? [Y/n]"
   "Are you ready for this ? [Y/n]"
   "Everything good ? [Y/n]"])

(def challenge-help-str
  "
Type a series of characters:

  `b` is `blue`
  `w` is `white`
  `r` is `red`
  `y` is `yellow`
  `o` is `black` (that's new :p)
  `g` is `green`

  I don't care about char case.

  also, quit with `q` or `quit` or `exit`.

  `h`|`help` display this help message
  ")

(def solver-help-str
  "
Type two numbers, each suffixed by a character:

  `o` represents the colors I guessed at the right place
  `x` represents the colors I guessed but at the wrong place.

  For example :
    If your code is:  blue  red    white blue
    and I give you:   black yellow blue  blue
    then you should give me: 1o 1x

    I win when you say `4o`

  I don't care about char case.

  also, quit with `q` or `quit` or `exit`.

  `h`|`help` display this help message
  ")

(def invalid-input-str
  "

  I don't get it.

  ")

(def win-str
  "

:::   :::  ::::::::  :::    :::      :::       ::: ::::::::::: ::::    :::
:+:   :+: :+:    :+: :+:    :+:      :+:       :+:     :+:     :+:+:   :+:
 +:+ +:+  +:+    +:+ +:+    +:+      +:+       +:+     +:+     :+:+:+  +:+
  +#++:   +#+    +:+ +#+    +:+      +#+  +:+  +#+     +#+     +#+ +:+ +#+
   +#+    +#+    +#+ +#+    +#+      +#+ +#+#+ +#+     +#+     +#+  +#+#+#
   #+#    #+#    #+# #+#    #+#       #+#+# #+#+#      #+#     #+#   #+#+#
   ###     ########   ########         ###   ###   ########### ###    ####

  ")


(def bye-str
  "
_________________________
                       /
    /                 /
---/__----------__---/---
  /   ) /   / /___) /
_(___/_(___/_(___ _o_____
          /
      (_ /

  ")

(defn- get-menu-val
  [i f]
  (f i))

(defn- print-opt
  [opt]
  (let [[lbl nm _] opt]
    (do
      (print lbl)
      (print " : ")
      (print nm)
      (println "")
      (flush))))

;; accessors

(defn challenge-header
  []
  challenge-header-str)

(defn challenge-help
  []
  challenge-help-str)

(defn solver-header
  []
  solver-header-str)

(defn solver-help
  []
  (solver-help-str))

(defn invalid-input
  []
  invalid-input-str)

(defn win
  []
  win-str)

;; ns-publics

(defn message
  "Just prints a message"
  [msg]
  (println msg))

(defn menu
  "Prints a menu from multiple options, and waits for user input.

  Options are vectors structured like so:
  `[opt-label opt-name opt-value]`

  Returns the `opt-value` matching the user input.
  "
  [& opts]
  (if (seq opts)
    (do
      (run!
        print-opt
        opts)
      (get-menu-val
        (read-line)
        (fn [i]
          (reduce
            (fn [acc opt]
              (let [[lbl _ v] opt]
                (if (= lbl i) v acc)))
            nil
            opts))))))

(defn code
  "Waits for user to input a code and returns the code
  or :help if user asked for help
  or :quit if user asked to quit
  or :invalid if the input is not legal
  "
  []
  (let [input (clojure.string/trim (read-line))]
    (cond
      (not input)
      :quit
      (re-matches #"(?i)h(elp)?" input)
      :help
      (re-matches #"(?i)q(uit)?|exit" input)
      :quit
      (re-matches #"(?i)[bwryog]{4}" input)
      (eng/string-to-code input)
      :else
      :invalid)))

(defn ready?
  "Waits for user to input a confirmation string (Y/n),
  or just enter to select the default option.

  Returns true if user wants to continue."
  []
  (let [input (clojure.string/trim (read-line))]
    (cond
      (not input)
      true
      (re-matches #"(?i)y(es)?" input)
      true
      :else
      false)))

(defn indications
  "Waits for user to input an indication and returns the indication map
  or :help if the user asked for help
  or :quit if the user asked to quit
  or :invalid if the input is not legal
  "
  []
  (let [input (clojure.string/trim (read-line))]
    (cond
      (clojure.string/blank? input)
      (eng/string-to-indications "0o 0x")
      (re-matches #"(?i)h(elp)?" input)
      :help
      (re-matches #"(?i)q(uit)?|exit" input)
      :quit
      (re-matches #"(?i)((\d+[xo])\s*){0,2}" input)
      (eng/string-to-indications input)
      :else
      :invalid)))


(defn prompt-code
  "Prints out a pretty prompt for a code"
  []
  (message (rand-nth prompts-code)))

(defn prompt-ready
  "Prints out a pretty prompt asking for confirmation of continuation"
  []
  (message (rand-nth prompts-ready)))

(defn indication
  "Displays indications in a pretty way.
  `hints` is a map of the following structure

  {
    :good ngood
    :bad nbad
  }
  "
  [hints]
  (let [ngood (get hints :good)
        nbad (get hints :bad)]
    (do
      (println "")
      (print "\tGOOD\t->\t" (or ngood 0) "\n")
      (print "\tBAD\t->\t" (or nbad 0) "\n")
      (println "")
      (flush))))


(defn start
  "Prints a pretty welcome message"
  []
  (println welcome-str)
  (flush))

(defn end
  "Prints a pretty good-bye message"
  []
  (println bye-str)
  (flush))

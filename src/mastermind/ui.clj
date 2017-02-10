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

(def prompts-code
  ["Penetrate my mind and read it!"
   "What am I thinking?"
   "Read me like an open book ;)"])

(def challenge-help-str
  "
Type a series of characters:

  `b` is `:blue`
  `w` is `:white`
  `r` is `:red`
  `y` is `:yellow`
  `o` is `black` (that's new :p)
  `g` is `green`

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
  (let [input (read-line)]
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

(defn prompt-code
  "Prints out a pretty prompt for a code"
  []
  (println (rand-nth prompts-code)))

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

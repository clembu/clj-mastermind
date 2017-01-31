(ns mastermind.core
  (:gen-class))

(defn answer [ans guess]
  (if (and (number? ans) (number? guess))
    (cond
      (= ans guess)
      :win
      (< ans guess)
      :high
      (> ans guess)
      :low
      :else
      :error)
    :error))

(defn react [ans]
  (answer ans (int (read))))

(defn play [ans]
  (loop [res :begin]
    (cond
      (= res :error)
      (println "There's been an error")
      (= res :win)
      (println "You guessed right!!")
      (= res :low)
      (or (println "Too low :p")
          (recur (react ans)))
      (= res :high)
      (or (println "Too high x)")
          (recur (react ans)))
      :else
      (or (println "Enter a number")
          (recur (react ans))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (play (rand-int 100)))

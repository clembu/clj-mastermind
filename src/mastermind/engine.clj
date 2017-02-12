(ns mastermind.engine)

(def color-vec [:red :blue :green :yellow :black :white])
(def color-set #{:red :blue :green :yellow :black :white})

(defn colors
  []
  color-set)

(defn get-code
  "Generates a random secret code of `n` colored pins
  (there are 6 colors)"
  [n]
  (loop [v [] n n]
   (cond
     (zero? n)
     v
     :else
       (recur
         (conj v (rand-nth color-vec))
         (dec n)))))

(defn indications
  "Gives indications reacting to a guess
  `t` is the guess.
  `code` is the code to guess."
  [code t]
  (loop [ind [] i 0]
   (cond
     (= (count t) i) ind
     (= (get t i) (get code i))
     (recur
       (conj ind :good)
       (inc i))
     (some (partial = (get t i)) code)
     (recur
       (conj ind :color)
       (inc i))
     :else
       (recur
         (conj ind :bad)
         (inc i)))))


(defn freq
  "Gives an associative structure representing the frequencies of the elements in `v`.

  The map is structured like so:
  {
    value: freq
  }

  Where `value` is an element of `v`
  and `freq` the number of times it appears in `v`"
  [v]
  (reduce
    (fn [acc e]
      (update acc e
        (fn [x]
          (if
            (contains? acc e)
            (inc (get acc e))
            1))))
    {}
    v))

(defn freqs-dispo
  "Gives an associative structure representing the available frequencies in the given `code` according to the given `ind`.

  The result is structured like so
  {
    value: freq
  }

  Where `value` is a color in the code
  and `freq` is the number of times it still happens in the code.
  "
  [code ind]
  (loop [code code ind ind f (freq code)]
   (cond
     (empty? code)
     f
     (= (get ind 0) :good)
     (recur
       (subvec code 1)
       (subvec ind 1)
       (assoc f (get code 0) (dec (get f (get code 0)))))
     :else
       (recur
         (subvec code 1)
         (subvec ind 1)
         f))))

(defn mdiff
  "Gives a map where each element is the difference of `f-a` and `f-b`"
  [f-a f-b]
  (into
    (sorted-map)
    (map
      (fn [k]
        (cond
          (contains? f-b k)
          [k (- (get f-a k) (get f-b k))]
          :else
          [k (get f-a k)]))
      (keys f-a))))

(defn filtre-indications
  "Filters out the indications so that if a color is no more needed, it is marked as `:bad`, instead of `:color`"
  [code guess ind]
  (loop [fdiff (mdiff (freq guess) (freq code)), guess guess, ind ind, acc []]
   (cond
     (empty? ind)
     acc
     (not (contains? fdiff (get guess 0)))
     (recur
       fdiff
       (subvec guess 1)
       (subvec ind 1)
       (conj acc (get ind 0)))
     (or
       (pos? (get fdiff (get guess 0)))
       (zero? (get fdiff (get guess 0)))
       (= (get ind 0) :good))
     (recur
       (assoc fdiff (get guess 0) (dec (get fdiff (get guess 0))))
       (subvec guess 1)
       (subvec ind 1)
       (conj acc (get ind 0)))
     :else
       (recur
         (assoc fdiff (get guess 0) (dec (get fdiff (get guess 0))))
         (subvec guess 1)
         (subvec ind 1)
         (conj acc :bad)))))

(defn char-to-col
  "Returns the color matching a character.

  `b` is `:blue`
  `w` is `:white`
  `r` is `:red`
  `y` is `:yellow`
  `o` is `:black` (new!)
  `g` is `:green`"
  [c]
  (case c
    \b :blue
    \w :white
    \r :red
    \y :yellow
    \o :black
    \g :green
    :invalid))

(defn char-to-ind
  "Returns the indication label matching a character.

  `o` is `:good`
  `x` is `:color`"
  [c]
  (case c
    \o :good
    \x :color
    :invalid))


(defn string-to-code
  "Returns the vector of the conversion of the characters of the string"
  [s]
  (vec (map char-to-col (seq (clojure.string/lower-case s)))))

(defn string-to-indications
  "Returns the indication map corresponding to the string"
  [s]
  (loop [matches (re-seq #"(?i)(\d+)(x|o)" s)
         ind {}]
    (if (seq matches)
      (let [[_ vs ks] (first matches)
            v (read-string vs)
            k (char-to-ind (first (clojure.string/lower-case ks)))]
        (recur (rest matches)
               (assoc ind k v)))
      ind)))

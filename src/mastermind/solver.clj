(ns mastermind.solver
  (:require [mastermind.engine :as eng]
            [clojure.zip :as zip]
            [clojure.pprint :as p]
            [mastermind.ui :as ui]))

(def initial-guess [:white :white :blue :blue])

(def root-value :root)

(defn- branch?
  [node]
  (vector? node))

(defn- value
  [node]
  (if (branch? node)
    (first node)
    node))

(defn- children
  [branch]
  (rest branch))

(defn- make-node
  [node children]
  (apply conj [] (first node) children))

(defn- zipper
  [root]
  (zip/zipper
    branch?
    children
    make-node
    root))

(defn- make-branch
  [color]
  [color])

(defn- make-leaf
  [color]
  color)

(defn- root?
  [zp]
  (= root-value (value (zip/node zp))))

(defn- possibilities-zip
  "Returns a zipper of all 1296 possible codes.

  Branch nodes of the zipper are vectors of the following shape:
  `[:{COLOR} &CHILDREN]`
  Leaf nodes are just colors

  The root node is [:root &CHILDREN]

  A node can then be either a vector of which `first` returns a color, or a color.
  "
  ([] (possibilities-zip 4 (eng/colors)))
  ([dpth col]
   (loop [zp (zipper (make-branch root-value))]
     (let [depth (- (dec dpth) (count (zip/path zp)))]
       (cond
         (zip/end? zp) (zipper (zip/root zp))
         (not (zip/branch? zp)) (recur (-> zp zip/next))
         (empty? (zip/children zp)) (recur
                                      (zip/replace
                                        zp
                                        (zip/make-node
                                          zp
                                          (zip/node zp)
                                          (map #(if (zero? depth)
                                                  %
                                                  [%])
                                               col))))
         :else (recur (-> zp zip/next)))))))

(defn- filter-value
  "Takes a predicate `pred?` and a zipper, and returns a new zipper, positionned at the root,
  with all nodes for which `(pred? (value node))` returns a truthy value removed. The root node is skipped."
  [pred? zp]
  (loop [z zp]
    (cond
      (root? z)
      (if (zip/end? z)
        (-> z zip/root zipper)
        (-> z zip/next recur))
      (pred? (value (zip/node z)))
      (-> z zip/remove zip/next recur)
      (zip/end? z)
      (-> z zip/root zipper)
      :else
      (-> z zip/next recur))))

(defn- filter-ordered
  "Takes a predicate `pred?` and a zipper, and returns a new zipper, positionned at the root,
  with all nodes for which `(pred? (value node) node-depth)` returns a truthy value removed. The root node is skipped."
  [pred? zp]
  (loop [z zp]
    (cond
      (root? z)
      (if (zip/end? z)
        (-> z zip/root zipper)
        (-> z zip/next recur))
      (pred? (value (zip/node z)) (count (zip/path z)))
      (-> z zip/remove zip/next recur)
      (zip/end? z)
      (-> z zip/root zipper)
      :else
      (-> z zip/next recur))))


;; PUBLIC

(defn next-guess
  "Takes the current tree of possible codes,
  and returns the next code to try."
  [zp]
  (vec
    (filter
      (partial not= root-value)
      (map
        value
        (let [z (-> zp zip/down zip/down zip/down zip/down)]
          (concat (zip/path z) [(zip/node z)]))))))

(defn update-tree
  "Takes the current tree of possible codes,
  the last attempted guess,
  and the indication map it got back from the player.

  Returns an updated tree of possible codes where all incompatible codes have been removed"
  [zp guess ind]
  (cond
    (= ind {}) ;; Entire guess was wrong
    (filter-value
      #(contains? (set guess) %)
      zp)

    :else
    zp))



(defn reaction
  "Takes an indication map

  The return value is either:
    - :win if the indication map is { :good 4 }
    - the next guess to try out
    - :impossible if there is no other possible guess to try"
  [ind]
  (if (= 4 (:good ind))
    :win
    (let [next ()])))

(defn playloop
  "The playloop of the solver mode.
  `zp` is the zipper of possibilities."
  [zp]
  (loop [z zp]))



(defn play
  "Play the solver mode.
  It's the alternative version where the computer guesses your code."
  []
  (ui/message (ui/solver-header))
  (ui/message (ui/solver-help))
  (ui/prompt-ready)
  (if (ui/ready?)
    (playloop (possibilities-zip))))

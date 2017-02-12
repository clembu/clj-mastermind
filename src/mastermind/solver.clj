(ns mastermind.solver
  (:require [mastermind.engine :as eng]
            [clojure.zip :as zip]
            [clojure.pprint :as p]))

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

(defn- filter-zip
  "Takes a predicate `pred?` and a zipper, and returns a new zipper, positionned at the root,
  with all nodes for which `(pred? (value node))` returns a truthy value removed, or nil if the `(pred? (value root))` returns true"
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


;; PUBLIC

(defn play
  "Play the solver game"
  []
  (println "Not ready yet"))

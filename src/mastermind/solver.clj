(ns mastermind.solver
  (:require [mastermind.engine :as eng]
            [clojure.zip :as zip]
            [clojure.pprint :as p]))

(defn- branch?
  [node]
  (vector? node))

(defn- value
  [node]
  (first node))

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

(defn- get-color
  [node]
  (if (branch? node)
    (value node)
    node))

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
   (loop [zp (zipper (make-branch :root))]
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

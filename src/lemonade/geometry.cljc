(ns lemonade.geometry
  (:refer-clojure :exclude [contains?])
  #?(:clj
     (:import [lemonade.core
               AffineTransformation Composite Region Frame Text Line Arc
               Circle]))
  (:require [lemonade.math :as math]
            [lemonade.core :as core
             #?@(:cljs
                 [:refer [AffineTransformation Frame Region Composite Text
                          Line Arc]])]))
(defprotocol Geometry
  (bound [shape]
    "Returns a bounding box for the shape.")
  (^boolean contained-in? [shape box]
    "Returns true iff shape overlaps visibly with the bounding box. Used to
    decide what needs to be rendered. Don't implement for polygons as
    Cohen-Sutherland is probably better than you.")
  (^boolean contains? [shape point]
   "Returns true iff shape contains point.")
  (distance [shape point]
    "Returns the distance from shape to point. Must return a positive number,
  and (zero? (distance shape point)) <=> (contains? shape point)"))

(defprotocol GeometricSet
  (-union [shape s2])
  (-intersection [shape s2]))

(defrecord NilBox []
    GeometricSet
  (-union [this o] o)
  (-intersection [this o] this))

(def empty-box (NilBox.))

(defrecord BoundingBox [xmin xmax ymin ymax]
  GeometricSet
  (-union [this o]
    (if (instance? NilBox o)
      this
      (BoundingBox. (min xmin (:xmin o))
                    (max xmax (:xmax o))
                    (min ymin (:ymin o))
                    (max ymax (:ymax o)))))

  Geometry
  (bound [box]
    box)
  (contains? [{:keys [xmin xmax ymin ymax]} [x y]]
    (and (<= xmin x xmax) (<= ymin y ymax))))

(defn union
  ([] empty-box)
  ([box] box)
  ([b1 b2]
   (-union ^not-native b1 b2))
  ([b1 b2 & more]
   (reduce -union (union b1 b2) more)))

(defn intersection
  ;; 0 args => infinite box. TODO: eventually
  ([box] box)
  ([b1 b2]
   (-intersection ^not-native b1 b2))
  ([b1 b2 & more]
   (reduce -intersection (intersection b1 b2) more)))

(defn bounding-box [[x1 y1] [x2 y2]]
  (BoundingBox. (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)))

(defn min-box
  "Returns the smallest bounding box around the convex hull of points."
  [points]
  (let [xs (map first points)
        ys (map second points)]
    (BoundingBox. (apply min xs) (apply max xs) (apply min ys) (apply max ys))))

(defn transform-bounding-box [atx {:keys [xmin ymin xmax ymax]}]
  (let [cps (map (partial math/apply-atx atx)
                 [[xmin ymin] [xmax ymax] [xmin ymax] [xmax ymin]])]
    (min-box cps)))

(defn pi-mults
  "Returns multiples of π/2 in interval [r s]"
  [r s]
  (if (<= (* 2 math/pi) (math/abs (- r s)))
    (map #(* (/ math/pi 2) %) (range 0 4))
    (let [[r s] (map #(mod % (* 2 math/pi)) [r s])]
      (filter #(<= r % s) (map #(* (/ math/pi 2) %) (range 0 8))))))

(extend-protocol Geometry
  #?(:clj Object :cljs default)
  (contains? [shape point]
    (zero? (distance shape point)))
  (bound [shape]
    (cond
      (core/template? shape)     (bound (core/expand-template shape))
      (sequential? shape)        (reduce union (map bound shape))
      (core/has-children? shape) (bound (core/children shape))
      :else                      empty-box))

  AffineTransformation
  (bound [{:keys [atx base-shape]}]
    (transform-bounding-box atx (bound base-shape)))

  Text
  (bound [{:keys [corner text]}]
    (bounding-box corner [(* 12 (count text)) 16]))

  Line
  (bound [{:keys [from to]}]
    (bounding-box from to))

  Arc
  (bound [{[x y] :centre from :from to :to r :radius}]
    (let [cf (math/cos from)
          ct (math/cos to)
          sf (math/sin from)
          st (math/sin to)]
      (min-box (conj (map (fn [x]
                            [(* r (math/cos x)) (* r (math/sin x))])
                          (pi-mults from to))
                     [(* r cf) (* r sf)]
                     [(* r ct) (* r st)])))))
(defn clean [node]
  (-> node
      (assoc :tag (gensym)
             (core/children-key node) nil)))

(defn recombinator [[head & tail]]
  (if (seq tail)
    (assoc head (core/children-key head) (recombinator tail))
    head))

(defn branch-seq*
  "Given a render tree, return a seq of all paths from the root to a leaf."
  [shape]
  (cond
    (core/has-children? shape)
    (let [node (clean shape)]
      (->> shape
           core/children
           branch-seq*
           (map #(conj % node))))

    (sequential? shape)
    (let [node (with-meta
                 (clean (core/composite))
                 (meta shape))]
      (->> shape
           (mapcat branch-seq*)
           (map #(conj % node))))

    :else
    (list (list shape))))

(defn branch-seq [shape]
  (map recombinator (branch-seq* shape)))

(defn effected-branches
  "Returns all branches of tree which contain point in their bounding boxes."
  [point tree]
  (->> tree
       branch-seq
       (filter #(contains? (bound %) point))))

(defn retree
  "Given a collection of branches that share the same root, reconstruct a
  subtree (potentially a subforest) of the original."
  [branches]
  (let [sets  (group-by first branches)
        trees (map (fn [[root branches]]
                     (cond-> (dissoc root :tag)
                       (core/has-children? root)
                       (assoc (core/children-key root)
                              (retree (map rest branches)))))
                   sets)]
    (map (fn [branch]
           (if (and (sequential? branch) (= 1 (count branch)))
             (first branch)
             branch))
         trees)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Index objects (not until speed becomes an issue)
;;
;; Tetrafurcation algo (2D B Tree):
;; Divide screen into quadrants, bucket images into quadrants, recur if any
;; quadrant has more than N objects (N to be tuned).
;;
;; To retrieve, pick the quadrant the point is in until we come to a bucket (no
;; more quadrants). Iterate through each VO in bucket and collect those that the
;; point is inside.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Winding number algo for second pass
;; http://geomalgorithms.com/a03-_inclusion.html
;; https://www.ams.org/journals/mcom/1973-27-122/S0025-5718-1973-0329216-1/S0025-5718-1973-0329216-1.pdf
;; Need to generalise these to beziers

(defmulti winding-angle (fn [point shape] (:type shape)))

(defmethod winding-angle :default
  [_ _]
  0)

(defmethod winding-angle ::core/path
  [point {:keys [contents]}]
  (reduce + (map winding-angle contents)))

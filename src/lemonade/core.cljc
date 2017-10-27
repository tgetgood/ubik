(ns lemonade.core
  #?(:cljs (:require-macros [lemonade.core :refer [deftemplate]]))
  (:require [clojure.pprint :refer [pprint pp]]
            [lemonade.geometry :as geometry :refer [atx cos idm pi sin]]))

(defmulti template-expand :type)

#?(:clj
   (defn namespace-qualified-kw [sym]
     (if (namespace sym)
       (keyword sym)
       (let [current-ns (namespace `x)]
         (keyword current-ns (name sym))))))

#?(:clj
   (defmacro deftemplate
     "Defines a new shape template. Something like a macro"
     [template-name template expansion]
     (let [fqkw (namespace-qualified-kw template-name)
           ks (keys (dissoc template :type))]
       `(do
          (def ~(symbol (name template-name))
            ~(assoc template :type template-name))
          (defmethod template-expand ~fqkw
            [{:keys [~@(map (comp symbol name) ks)] :as in#}]
            (let [~'style (or ~'style {})]
              ~expansion))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Core Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: The line and bezier here are not very reusable. They're a royal pain
;; to manipulate via affine transformations. Path segments are manipulated by
;; their boundaries, not as shapes in the plane.
;;
;; Maybe there's a fundamental distinction to be made between path segments and
;; everything else?

(def line
  {:type ::line
   :from [0 0]
   :to [1 1]})

(def bezier
  "Bezier cubic to be precise."
  {:type ::bezier
   :from [0 0]
   :c1 [0 0]
   :c2 [1 1]
   :to [1 1]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Higher Order Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: It would be nice if these were types that one could conj onto. As is
;; I'm implementing custom conj for each anyway...
(defn path
  ([segments] (path {} segments))
  ([style segments]
   (let [closed? (or (:closed (meta segments))
                     (geometry/closed? segments))]
     {:type ::path
      :closed? closed?
      :style style
      :contents segments})))

(defn conj-path [{:keys [style contents]} segment]
  (path style (conj contents segment)))

(defn composite
  ([shapes] (composite {} shapes))
  ([style shapes]
   {:type ::composite
    :style style
    :contents shapes}))

(defn with-style [style & shapes]
  (composite style shapes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shape Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate circle
  {:style {} :radius 1 :centre [0 0]}
  (path style ^:closed [{:type   ::arc
                         :centre centre
                         :radius radius
                         :from   0
                         :to     {:unit :radians :angle (* 2 pi)}}]))

(deftemplate ::polyline
  {:style {} :points []}
  (path style (mapv (fn [[x y]]
                      {:type ::line
                       :from x
                       :to y})
                    (partition 2 (interleave points (rest points))))))

(deftemplate ::rectangle
  {:style  {}
   :corner [0 0]
   :height 1
   :width  1}
  (let [[x1 y1] corner
        x2      (+ x1 width)
        y2      (+ y1 height)]
    {:type ::polyline
     :points [[x1 y1] [x2 y1] [x2 y2] [x1 y2] [x1 y1]]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Affine Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translation
  "Returns an affine transformation corresponding to translation by b"
  [b]
  (atx idm b))

(defn rotation
  "Returns a counterclockwise rotation about the origin by angle (linear).
  Note: angle in degrees."
  [angle]
  (let [r (geometry/deg->rad angle)
        c (cos r)
        s (sin r)]
    (atx [c (- s) s c])))

(defn scaling
  "Returns a linear map which scales by [x y] in the x and y directions"
  [[x y]]
  (atx [x 0 0 y]))

(defn reflection
  "Returns a reflection about vector v (linear)"
  [[x y]]
  (if (= 0 x)
    (atx [-1 0 0 1])
    (let [m    (/ y x)
          m2   (* m m)
          m2+1 (inc m2)
          diag (/ (- 1 m2) m2+1)
          off  (/ (* 2 m) m2+1)]
       (atx [diag off off (- diag)]))))

(defn recentre
  "Given a linear transformation and a point, return an affine transformation
  corresponding to the transformation about the point."
  [origin atx]
  (geometry/comp-atx
   (translation origin)
   atx
   (translation (map - origin))))

;;;;; Applied affine txs

(defn transform
  "Returns a new shape which is the given affine map applies to the base shape."
  [base atx]
  {:type ::atx
   :base-shape base
   :atx atx})

(defn translate
  "Returns a copy of shape translated by [x y],"
  [b shape]
  (transform shape (translation b)))

(defn rotate
  "Returns a copy of shape rotated by angle around the given centre of
  rotation."
  ([angle shape] (rotate [0 0] angle shape))
  ([centre angle shape]
   (transform shape (recentre centre (rotation angle)))))

(defn scale
  "Returns a copy of shape scaled horizontally by a and verticaly by b. Centre
  is the origin (fixed point) of the transform."
  ([a shape]
   (scale [0 0] a shape))
  ([centre a shape]
   (let [extent (if (vector? a) a [a a])]
     (transform shape (recentre centre (scaling extent))))))

(defn reflect
  "Returns a copy of shaped reflected around the line with direction dir through
  centre."
  ([dir shape] (reflect [0 0] dir shape))
  ([centre dir shape]
   (transform shape (recentre centre (reflection dir)))))

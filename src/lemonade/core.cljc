(ns lemonade.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [lemonade.spec-gen :as spec-gen]
            [lemonade.style :as style]
            [lemonade.geometry :as geometry :refer [atx cos idm pi sin]]))

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

;;;;; Shape Templates

(defn circle-template [{:keys [radius centre style]}]
  ;; REVIEW: Should the style go on the path, or on the segment?
  ;; What's the difference?
  {:style style
   :segments [{:type ::arc
               :centre centre
               :radius radius
               :from 0
               :to {:unit :radians :angle (* 2 pi)}}]})

(def circle
  "Unit circle"
  {:type ::circle
   :centre [0 0]
   :radius 1})

(def square
  "Unit square"
  {:type ::square
   :corner [0 0]
   :width 1})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Affine Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translation
  "Returns an affine transformation corresponding to translation by b"
  [b]
  (atx idm b))

(defn rotation
  "Returns a counterclockwise rotation about the origin by angle (linear)"
  [angle]
  (let [r (geometry/parse-angle angle)
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
  [(translation (map - origin))
   atx
   (translation origin)])

;;;;; Applied affine txs

(defn transform
  "Returns a new shape which is the given affine map applies to the base shape."
  [base atx]
  {:base-shape base
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Paths

(s/def ::line
  (s/keys :req-un [::geometry/from ::geometry/to]  :opt-un [::style/style]))

(s/def ::bezier
   (s/keys :req-un [::geometry/from ::geometry/to ::geometry/c1 ::geometry/c2]
           :opt-un [::style/style]))

(defmulti path-segment :type)
(defmethod path-segment ::line [_] ::line)
(defmethod path-segment ::bezier [_] ::bezier)
(s/def ::path-segment (s/multi-spec path-segment :type))

(s/def ::segments
  (s/with-gen
    (s/and (s/coll-of ::path-segment :kind sequential? :min-count 2)
           geometry/connected?)
    spec-gen/segment-gen))

(s/def ::path
  (s/or :single-segment  ::path-segment
        :joined-segments (s/keys :req-un [::segments] :opt-un [::style/style])))

;;; Surfaces 2d
;;
;; Templates? Macros? What the hell are these going to be exactly?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REVIEW: How to distinguish the circle from the disc? A circle is technically
;; a 1d object, a path. A circle's interior is a disc which is a surface. Is
;; this pedantic or important?
(s/def ::circle
  (s/keys :req-un [::geometry/centre ::geometry/radius] :opt-un [::style/style]))

(s/def ::square
  (s/with-gen
    (s/and (s/keys :req-un [::geometry/corner
                            (or ::geometry/width
                                ::geometry/height)]
                   :opt-un [::style/style])
           (s/or :no-width  #(-> % :width nil?)
                 :no-height #(-> % :height nil?)
                 :equal     #(= (:width %) (:height %))))
    spec-gen/square-gen))

(defmulti shape-template :type)
(defmethod shape-template ::circle [_] ::circle)
(defmethod shape-template ::square [_] ::square)

(s/def ::shape-template (s/multi-spec shape-template :type))

(s/def ::primitive-shape
  (s/or :path     ::path
        :template ::shape-template))

(s/def ::shape
  (s/or :primitive   ::primitive-shape
        :composite   ::composite
        :transformed ::affine-transform))

(s/def ::shapes
  (s/coll-of ::shape :kind sequential?))

(s/def ::composite
  (s/keys :req-un [::shapes] :opt-un [::style/style]))

;;;;; Affine Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::affine-transformation
  (s/keys :req-un [::geometry/matrix ::geometry/translation]))

(s/def ::composed-atx
  (s/coll-of ::atx :kind sequential?))

(s/def ::atx
  (s/or :composition ::composed-atx
        :single ::affine-transformation))

(s/def ::base-shape ::shape)

(s/def ::affine-transform
  (s/keys :req-un [::atx ::base-shape]))

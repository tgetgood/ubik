(ns lemonade.core
  (:require [lemonade.spec :as s]))

;;;;; Core shapes

;; REVIEW: The line and bezier here are not very reusable. They're a royal pain
;; to manipulate via affine transformations. Path segments are manipulated by
;; their boundaries, not as shapes in the plane.
;;
;; Maybe there's a fundamental distinction to be made between path segments and
;; everything else?
(def line
  {:type ::s/line
   :from [0 0]
   :to [1 1]})

(def bezier
  "Bezier cubic to be precise."
  {:type ::s/bezier
   :from [0 0]
   :c1 [0 0]
   :c2 [1 1]
   :to [1 1]})

(def circle
  "Unit circle"
  {:type ::s/circle
   :centre [0 0]
   :radius 1})

(def square
  "Unit square"
  {:type ::s/square
   :corner [0 0]
   :width 1})

;;;;; Geometry

(defn deg->rad [d]
  (* #?(:cljs js/Math.PI
        :clj Math/PI)
     (/ d 180)))

(defn parse-angle [a]
  (if (number? a)
    (deg->rad a)
    (let [{:keys [units amount]} a]
      (cond
        (= units :degrees) (deg->rad amount)
        (= units :radians) amount
        :else nil))))

(defn sin [x]
  (#?(:cljs js/Math.sin :clj Math/sin) x))

(defn cos [x]
  (#?(:cljs js/Math.cos :clj Math/cos) x))

;;;;; Affine Transforms

(def ^:private idm
  "The 2x2 identity matrix"
  [1 0 0 1])

(defn det
  "Returns the determinant of a 2x2 matrix"
  [a b c d]
  (- (* a d) (* b c)))

(defn- atx
  "Convenience fn for building atx maps"
  ([m]
   (atx m [0 0]))
  ([m b]
   {:matrix      m
    :translation b}))

(defn invert-atx
  "Returns matrix corresponding to the inverse affine transform."
  [{[a b c d] :matrix [x y] :translation}]
  (let [abs (det a b c d)
        [a' b' c' d'] (map #(/ % abs) [d (- b) (- c) a])
        x' (- (+ (* a' x) (* c' y)))
        y' (- (+ (* b' x) (* d' y)))]
    (atx [a' b' c' d'] [x' y'])))

(defn translation
  "Returns an affine transformation corresponding to translation by b"
  [b]
  (atx idm b))

(defn rotation
  "Returns a counterclockwise rotation about the origin by angle (linear)"
  [angle]
  (let [r (parse-angle angle)
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

(defn- wrap-atx
  [b atx]
  {:base-shape b
   :atx atx})

(defn translate
  "Returns a copy of shape translated by [x y],"
  [b shape]
  (wrap-atx shape (translation b)))

(defn rotate
  "Returns a copy of shape rotated by angle around the given centre of
  rotation."
  ([angle shape] (rotate [0 0] angle shape))
  ([centre angle shape]
   (wrap-atx shape (recentre centre (rotation angle)))))

(defn scale
  "Returns a copy of shape scaled horizontally by a and verticaly by b. Centre
  is the origin (fixed point) of the transform."
  ([a shape]
   (scale [0 0] a shape))
  ([centre a shape]
   (let [extent (if (vector? a) a [a a])]
     (wrap-atx shape (recentre centre (scaling extent))))))

(defn reflect
  "Returns a copy of shaped reflected around the line with direction dir through
  centre."
  ([dir shape] (reflect [0 0] dir shape))
  ([shape centre dir]
   (wrap-atx shape (recentre centre (reflection dir)))))

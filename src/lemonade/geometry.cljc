(ns lemonade.geometry
  (:require [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [lemonade.geometry :refer [derive-spec]])))

;;;;; Trig

(def π
  #?(:cljs js/Math.PI
     :clj Math/PI))

(def pi
  "Ratio of circumference to diameter of a circle.
  For those who don't like programming with unicode."
  π)

(defn deg->rad
  "Converts degrees to radians"
  [d]
  (* π (/ d 180)))

(defn sin [x]
  (#?(:cljs js/Math.sin :clj Math/sin) x))

(defn cos [x]
  (#?(:cljs js/Math.cos :clj Math/cos) x))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

;;;;;; Path topology

(defn connected?
  "Returns true if the sequential of paths passed in are pairwise connected."
  ;; TODO: spec
  [[x & more]]
  (if (empty? more)
    true
    (let [y (first more)]
      (and (= (:to x) (:from y)) (recur more)))))

(defn closed-segment?
  [path]
  (and (= :lemonade.core/arc (:type path))
       (< (* 2 π) (abs (- (:to path) (:from path))))))

(defn closed-path?
  "Returns true if paths form the boundary of a connected surface.
  Technically I'm requiring a connecting bridge of non-zero measure. Not sure if
  that's a good call...
  Simply connected not necessary, just no point connections."
  [paths]
  ;; TODO: spec.
  (and (connected? paths) (= (:from (first paths)) (:to (last paths)))))

(defn closed?
  ;; FIXME: This is ugly. I need to move topology to a higher level. I'm going
  ;; to bury myself in special cases this way.
  [paths]
  (or (closed-path? paths)
      (every? closed-segment? paths)))

;;;;; Linear and Affine

(def idm
  "The 2x2 identity matrix"
  [1 0 0 1])

(defn det
  "Returns the determinant of a 2x2 matrix"
  [a b c d]
  (- (* a d) (* b c)))

(defn atx
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

(defn comp-atx
  "Returns the composition of affine transformations"
  ([{[a b c d] :matrix [x y] :translation}
    {[a' b' c' d'] :matrix [x' y'] :translation}]
   (atx [(+ (* a a') (* b c'))
         (+ (* a b') (* b d'))
         (+ (* c a') (* d c'))
         (+ (* c b') (* d d'))]
        [(+ x (* a x') (* b y'))
         (+ y (* c x') (* d y'))]))
  ([a b & more] (reduce comp-atx (comp-atx a b) more)))

;;;;; Utils for core
;; REVIEW: (should maybe be in core?)

(defn parse-angle [a]
  (if (number? a)
    (deg->rad a)
    (let [{:keys [units amount]} a]
      (cond
        (= units :degrees) (deg->rad amount)
        (= units :radians) amount
        :else nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Specs
;;
;; I think these belong here since they're actually part of the application
;; code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro derive-spec
  "Shorthand for repeated derivations"
  {:style/indent 1}
  [spec & keys]
  `(do
     ~@(for [key keys]
         (list 's/def key spec))))

;;; Scalars

(defn nan? [x]
  #?(:clj (Double/isNaN x)
     :cljs (js/isNaN x)))

(defn finite? [x]
  (and
   (not= x ##Inf)
   (not= x ##-Inf)))

;; Real numbers
(s/def ::real
  (s/and number?
         finite?
         (comp not nan?)))

(s/def ::scalar ::real)
(s/def ::non-negative (s/and ::scalar (comp not neg?)))

(derive-spec ::non-negative
  ::radius
  ::width
  ::length
  ::height)

(s/def ::unit #{:radians :degrees})
(s/def ::angle ::scalar)

(s/def ::angle-with-unit
  (s/keys :req-un [::unit ::angle]))

(s/def ::general-angle
  (s/or :unitless ::scalar
        :with-unit ::angle-with-unit))

(s/def :lemonade.geometry.angle/from ::general-angle)
(s/def :lemonade.geometry.angle/to ::general-angle)

;;; Points 0d

(s/def ::point
  (s/coll-of ::scalar :kind vector? :count 2))

(s/def ::points
  (s/coll-of ::point :kind vector?))

(derive-spec ::point
  ::from
  ::to
  ::centre
  ::corner
  ::c1
  ::c2)

;;;;; Affine stuff

(s/def ::vector (s/coll-of ::scalar :kind sequential? :count 2))

;; (a b c d), canvas takes them as (a c b d) because of silliness.
(s/def ::matrix (s/coll-of ::scalar :kind sequential? :count 4))

(s/def ::translation ::vector)

;;;;; fn specs
;; TODO: Relocate

(s/fdef parse-angle
        :args (s/cat :angle ::general-angle)
        :ret ::real)

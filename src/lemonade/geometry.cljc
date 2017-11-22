(ns lemonade.geometry)

;;;;; Trig

(def π
  #?(:cljs js/Math.PI
     :clj Math/PI))

(def pi
  "Ratio of circumference to diameter of a circle.
  For those who don't like programming with unicode."
  π)

(def e
  #?(:clj Math/E
     :cljs js/Math.E))

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

(defn sqrt [x]
  (#?(:cljs js/Math.sqrt :clj Math/sqrt) x))

(defn nan? [x]
  (#?(:cljs js/isNaN :clj Double/isNaN) x))

(defn exp
  "Exponential function. Returns b^n. If b not specified defaults to Euler's
  number."
  ;; REVIEW: For small n, there's expm1 which can be much more accurate. I
  ;; should look into that for windowing effects.
  ([n]
   (#?(:clj Math/exp :cljs js/Math.exp) n))
  ([b n]
   (#?(:clj Math/pow :cljs js/Math.pow) b n)))

(defn dot [x y]
  (reduce + (map * x y)))

(defn norm [[x y]]
  (+ (* x x) (* y y)))

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

(def ^:private **
  "Temporary workaround for spec and big arithmetic"
  #?(:clj *' :cljs *))

(def idm
  "The 2x2 identity matrix"
  [1.0 0.0 0.0 1.0])

(defn det
  "Returns the determinant of a 2x2 matrix"
  [a b c d]
  (- (** a d) (** b c)))

(defn atx
  "Convenience fn for building atx maps"
  ([m]
   (atx m [0 0]))
  ([m b]
   {:matrix      m
    :translation b}))

(def id (atx idm [0.0 0.0]))

(defn invert-atx
  "Returns matrix corresponding to the inverse affine transform."
  [{[a b c d] :matrix [x y] :translation}]
  (let [abs (det a b c d)
        [a' b' c' d'] (map #(/ % abs) [d (- b) (- c) a])
        x' (- (+ (** a' x) (** b' y)))
        y' (- (+ (** c' x) (** d' y)))]
    (atx [a' b' c' d'] [x' y'])))

(defn comp-atx
  "Returns the composition of affine transformations"
  ([] id)
  ([a] a)
  ([{[a b c d] :matrix [x y] :translation}
    {[a' b' c' d'] :matrix [x' y'] :translation}]
   (atx [(+ (* a a') (* b c'))
         (+ (* a b') (* b d'))
         (+ (* c a') (* d c'))
         (+ (* c b') (* d d'))]
        [(+ x (* a x') (* b y'))
         (+ y (* c x') (* d y'))]))
  ([a b & more] (reduce comp-atx (comp-atx a b) more)))

(defn apply-atx
  "Applies affine tx to a point and returns the result."
  [{[a b c d] :matrix [e f] :translation} [x y]]
  [(+ (* a x) (* b y) e) (+ (* c x) (* d y) f)])

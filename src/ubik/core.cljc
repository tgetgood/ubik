(ns ubik.core
  #?(:cljs (:require-macros [ubik.core :refer [deftemplate]]))
  (:require [clojure.string :as string]
            [ubik.math :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ITemplate
  (expand-template [this] "Returns the expansion of this template"))

(defn type-case
  "Returns a symbol in idiomatic TypeCase given one in clojure standard
  symbol-case."
  [sym]
  (symbol (apply str (map string/capitalize (string/split (name sym) #"-")))))

#?(:clj
   (defmacro deftemplate
     "Defines a new shape template. Creates a new record whose name is
  instance-name converted to UpperCamelCase as per record naming conventions.

  The canonical instance of the new template will be bound to instance-name.

  expansion will be executed in an environment when all keys of the template
  name have been bound to symbols. Expansions must return a valid shape
  (template or otherwise).

  Optionally impls are protocol implementations as per defrecord."
     [instance-name template expansion & impls]
     (let [template-name (type-case instance-name)
           fields (map (comp symbol name) (keys template))]
       `(do
          ;; TODO: I can generate a spec from the field list and then check it's
          ;; valid at expansion time. I think that would be a good place to find
          ;; errors.
          ;;
          ;; The problem is that adding a spec/def into this macro expansion
          ;; causes the whole thing to go haywire even though the relevant parts
          ;; of the expansion don't change at all...
          (defrecord ~template-name [~@fields]
            ubik.core/ITemplate
            (ubik.core/expand-template [this#]
              ~expansion)
            ~@impls)
          (def ~instance-name
            (~(symbol (str "map->" template-name)) ~template))))))

(defn ^boolean template? [shape]
  (satisfies? ITemplate shape))

(defn template-expand-all [shape]
  (if (template? shape)
    (recur (expand-template shape))
    shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Indexing / Location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tag
  "Tags shape with key so that it can be looked up later. Key can be either a
  single value or a vector. If a vector is provided the index will be nested."
  ([shape key] (tag shape key {}))
  ([shape key metadata]
   (with-meta shape
     (update (meta shape) ::tag merge {key metadata}))))

(defn get-tags
  "Returns all tags which have been added to this shape."
  [shape]
  (into #{} (map key (get (meta shape) ::tag))))

(defn get-tag-data [shape key]
  (-> shape
      meta
      (get ::tag)
      (get key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shape Universals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IShape
  (children-key [shape]))

(defn has-children? [shape]
  (not (nil? (children-key shape))))

(defn children [shape]
  (get shape (children-key shape)))

(defn update-children [shape f]
  (update shape (children-key shape) f))

(defn walk-down [shape f]
  (let [f (fn [s] (with-meta (f s) (meta s)))]
    (cond
      (sequential? shape)
      (with-meta
        (into (empty shape) (map f shape))
        (meta shape))

      (has-children? shape)
      (f (update shape (children-key shape) f))

      :else
      (f shape))))

(extend-protocol IShape
  nil
  (children-key [_] nil)

  #?(:cljs default :clj Object)
  (children-key [_]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; 1D Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISegment
  (endpoints [this]
    "Returns the endpoints of this segment in order (segments are directed
    paths.")
  (contiguous [this] "Returns true iff this segment is connected."))

;;;;;; Path topology

(defn connected?
  "Returns true if the sequential of paths passed in are pairwise connected."
  [paths]
  (loop [[path & more] paths]
    (if (seq more)
      (when (= (:to path) (:from (first more)))
        (recur more))
      true)))

(defn closed-segment?
  [path]
  (and (contiguous path)
       (let [[start end] (endpoints path)]
         (= start end))))

(defn closed-path?
  "Returns true if paths form the boundary of a connected surface.
  Technically I'm requiring a connecting bridge of non-zero measure. Not sure if
  that's a good call...
  Simply connected not necessary, just no point connections."
  [paths]
  (if (= 1 (count paths))
    (closed-segment? (first paths))
    (let [points (map (fn [[f t]] {:from f :to t}) (map endpoints paths))]
      (when (connected? paths)
        (= (:from (first paths)) (:to (last paths)))))))

;;;;; Path Segments

(defrecord Line [style from to]
  ISegment
  (endpoints [_] [from to])
  (contiguous [_] true))

(def line
  (map->Line
   {:from [0 0]
    :to [1 1]}))

(defrecord Bezier [style from to c1 c2]
  ISegment
  (endpoints [_] [from to])
  (contiguous [_] true))

(def bezier
  "Bezier cubic to be precise."
  (map->Bezier
   {:from [0 0]
    :c1 [0 0]
    :c2 [1 1]
    :to [1 1]}))

(defrecord Arc [style centre radius from to clockwise?]
  ISegment
  (endpoints [_]
    ;; FIXME: assumes |from - to| <= 2π
    (->> [from to]
         (map (juxt math/cos math/sin))
         (map (partial map (partial * radius)))
         (mapv (partial mapv + centre))))
  (contiguous [_] true))

(def arc
  (map->Arc
   {:centre [0 0]
    :radius 1
    :from   0
    :to     (* 2 math/pi)
    :clockwise? false}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Higher Order Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Region [style boundary]
  IShape
  (children-key [_] :boundary))

(defn region
  ([boundary] (region {} boundary))
  ([style boundary]
   (map->Region
    {:style style
     :boundary boundary})))

(defrecord Composite [style contents]
  IShape
  (children-key [_] :contents))

(defn composite
  ([] (composite nil nil))
  ([contents] (composite nil contents))
  ([style contents]
   (map->Composite {:style style
                    :contents contents})))

(defrecord Frame [corner width height style base-shape]
  IShape
  (children-key [_] :base-shape))

(def frame
  "A frame is a visual which restricts image to fall within a rectangle.
  Keys as per ubik.core/rectangle plus the :base-shape which is framed."
  (map->Frame
   {:corner [0 0]
    :width 1
    :height 1
    :style {}
    :base-shape []}))

(defn with-style [style & shapes]
  (composite style shapes))

;; REVIEW: Are both of these used?
(defn style [shape style]
  (with-style style shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Core Template Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn full-arc [c r & [cw?]]
  (assoc arc
         :centre c
         :radius r
         :from   0
         :to     (* 2 math/pi)
         :clockwise? cw?))

(deftemplate circle
  {:style {} :radius 1 :centre [0 0]}
  (region style [(full-arc centre radius)]))

(deftemplate annulus
  {:style {} :inner-radius 1 :outer-radius 2 :centre [0 0]}
  (region style
          [(full-arc centre inner-radius)
           (full-arc centre outer-radius true)]))

(deftemplate polyline
  {:style {} :points []}
  (let [segs (map (fn [[x y]]
                    (assoc line
                           :from x
                           :to   y))
                   (partition 2 (interleave points (rest points))))
        closed? (= (first points) (last points))]
    (region style segs)))

(deftemplate rectangle
  {:style  {}
   :corner [0 0]
   :height 1
   :width  1}
  (let [[x1 y1] corner
        x2      (+ x1 width)
        y2      (+ y1 height)]
    (assoc polyline
           :style style
           :points [[x1 y1] [x2 y1] [x2 y2] [x1 y2] [x1 y1]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Affine Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Affine Maps

(defn translation
  "Returns an affine transformation corresponding to translation by b"
  [b]
  (math/atx math/idm b))

(defn rotation
  "Returns a counterclockwise rotation about the origin by angle (linear).
  Note: angle in degrees."
  [angle]
  (let [r (math/deg->rad angle)
        c (math/cos r)
        s (math/sin r)]
    ;; Rotation matrix corrected for coordinate inversion.
    ;; N.B.: If we ever have a host that does not invert, some refactoring will
    ;; be in order
    (math/atx [c s s (- c)])))

(defn scaling
  "Returns a linear map which scales by [x y] in the x and y directions"
  [[x y]]
  (math/atx [x 0 0 y]))

(defn reflection
  "Returns a reflection about vector v (linear)"
  [[x y]]
  (if (zero? x)
    (math/atx [-1 0 0 1])
    (let [m    (/ y x)
          m2   (* m m)
          m2+1 (inc m2)
          diag (/ (- 1 m2) m2+1)
          off  (/ (* 2 m) m2+1)]
       (math/atx [diag off off (- diag)]))))

(defn recentre
  "Given a linear transformation and a point, return an affine transformation
  corresponding to the transformation about the point."
  [origin atx]
  (math/comp-atx
   (translation origin)
   atx
   (translation (map - origin))))

;;;;; Applied Affine Transformations

(defrecord AffineTransformation [atx base-shape]
  IShape
  (children-key [_] :base-shape))

(defn transform
  "Returns a new shape which is the given affine map applies to the base shape.
  N.B.: If the atx is degenerate (det = 0) then an empty shape is returned."
  ;; REVIEW: This is technically incorrect since shapes can be collapsed to 1
  ;; dimension, and would thus still have a (degenerate but drawable)
  ;; boundary. Is there any valid use case here?
  [shape atx]
  (if (zero? (apply math/det (:matrix atx)))
    []
    (map->AffineTransformation
     {:base-shape shape
      :atx atx})))

(defn translate
  "Returns a copy of shape translated by [x y],"
  [shape b]
  (with-meta
    (transform shape (translation b))
    ;; REVIEW: These annotations both provide for human readable code and a non
    ;; trivial optimisation on any linear algebra involved (especially
    ;; rendering). So I think it would be wise to make record types for the
    ;; different atxes, but the questions arises: Should there be several types
    ;; of affine transformation applied to a shape, or different types of affine
    ;; map (in the math ns) any of which could be bound to the :atx key of an
    ;; AffineTransformation?
    ;;
    ;; It feels like a trivial difference, and maybe it is, but let's just leave
    ;; it be for a bit and maybe it'll become clear.
    {:atx-type :translation :point b}))

(defn rotate
  "Returns a copy of shape rotated by angle around the given centre of
  rotation."
  ([shape angle] (rotate shape [0 0] angle))
  ([shape centre angle]
   (with-meta
     (transform shape (recentre centre (rotation angle)))
     {:atx-type :rotation :point centre :angle angle})))

(defn scale
  "Returns a copy of shape scaled horizontally by a and verticaly by b. Centre
  is the origin (fixed point) of the transform."
  ([shape a]
   (scale shape [0 0] a))
  ([shape centre a]
   (let [extent (if (vector? a) a [a a])]
     (with-meta
       (transform shape (recentre centre (scaling extent)))
       {:atx-type :scale :point centre :extent extent}))))

(defn reflect
  "Returns a copy of shaped reflected around the line with direction dir through
  centre."
  ([shape dir] (reflect shape [0 0] dir))
  ([shape centre dir]
   (with-meta
     (transform shape (recentre centre (reflection dir)))
     {:atx-type :reflection :point centre :line dir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord RawText [style corner text])

(def raw-text
  "Single line of text. No wrapping or truncation."
  (map->RawText
   {:style  {:font "sans serif 10px"}
    :corner [0 0]
    :text   ""}))

;; Renders text fit for the global coord transform (origin in the bottom left)
(deftemplate text
  {:style {:font "sans serif 10px"}
   :corner [0 0]
   :text ""}
  (-> raw-text
      (assoc :text text :style style :corner corner)
      (reflect corner [1 0])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Shape Walking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sequential-types
  "Reference. This needs to be included literally pretty much everywhere it's
  used because macros always read the :clj form regardless of which runtime you
  mean."
  #?(:cljs [List
            LazySeq
            PersistentVector
            IndexedSeq
            ArrayList]
     :clj [clojure.lang.PersistentVector
           clojure.lang.PersistentList
           clojure.lang.ArraySeq
           clojure.lang.IndexedSeq
           clojure.lang.LazySeq]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw!
  "Draws shape to host. The host determines what drawing means. Return value
  isn't generally meaningful."
  [shape host]
  ((:render-fn host)
   (with-meta
     (transform shape (math/atx [1 0 0 -1] [0 (:height host)]))
     {:atx-type ::coordinate-inversion})))

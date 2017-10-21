(ns lemonade.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint pp]]))

;;;;; Styles

(s/def ::style
  (s/keys :req-un []))

;;;;; Geometry

;;; Scalars

(s/def ::scalar number?)
(s/def ::non-negative (s/and ::scalar (comp not neg?)))

(s/def ::radius ::non-negative)

;;; Points 0d

(s/def ::point
  (s/and (s/coll-of ::scalar :kind sequential? :count 2)))

;;; Names for points.
;; REVIEW: It seems a little weird to have so many aliases, but then we always
;; use reams of symbols to represent the same *kind* of thing in equations...
(s/def ::from ::point)
(s/def ::to ::point)
(s/def ::centre ::point)
(s/def ::c1 ::point)
(s/def ::c2 ::point)

;;; Paths 1d

(s/def ::type keyword?)

(defmulti path-segment ::type)
(s/def ::path-segment (s/multi-spec path-segment ::type))

(defmethod path-segment ::line [_]
  (s/keys :req [::type] :req-un [::from ::to]  :opt-un [::style]))

(defmethod path-segment ::bezier [_]
  (s/keys :req [::type] :req-un [::from ::to ::c1 ::c2] :opt-un [::style]))

(defn connected?
  "Returns true if the sequential of paths passed in are pairwise connected."
  ;; TODO: spec
  [[x & more]]
  (if (empty? more)
    true
    (let [y (first more)]
      (and (= (:to x) (:from y)) (recur more)))))

(defn closed?
  "Returns true if paths form the boundary of a connected surface.
  Technically I'm requiring a connecting bridge of non-zero measure. Not sure if
  that's a good call...'"
  [paths]
  ;; TODO: spec.
  (and (connected? paths) (= (:from (first paths)) (:to (last paths)))))

(s/def ::segments
  (s/and (s/coll-of ::path-segment :kind sequential? :min-count 1) connected?))

(s/def ::path
  (s/keys :req-un [::segments] :opt-un [::style]))

;;; Surfaces 2d

;; REVIEW: How to distinguish the circle from the disc? A circle is technically
;; a 1d object, a path. A circle's interior is a disc which is a surface. Is
;; this pedantic, or important?
(s/def ::circle
  (s/keys :req-un [::centre ::radius] :opt-un [::style]))

;;; No 3d yet

;;;;; Affine Transforms

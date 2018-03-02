(ns ubik.spec.core
  (:require [clojure.spec.alpha :as s]
            [ubik.core :as core]
            [ubik.spec.gen :as gen]
            [ubik.spec.math :as math]
            [ubik.spec.style :as style]))

(s/def ::line
  (s/keys :req-un [::math/from ::math/to]  :opt-un [::style/style]))

(s/def ::bezier
   (s/keys :req-un [::math/from ::math/to ::math/c1 ::math/c2]
           :opt-un [::style/style]))


(s/def ::arc
  (s/keys :req-un [::math/centre ::math/radius :ubik.math.angle/to
                   :ubik.math.angle/from ::clockwise?]
          :opt-un [::style/style]))

(defmulti path-segment :type)
(defmethod path-segment ::core/line [_] ::line)
(defmethod path-segment ::core/bezier [_] ::bezier)
(defmethod path-segment ::core/arc [_] ::arc)

(s/def ::segment (s/multi-spec path-segment :type))

(s/def ::segments
  (s/coll-of ::segment :kind sequential?))

;; REVIEW: A boundary is a list of segments with extra semantics. That's
;; captured in ::region, but is that a good place?
(s/def ::boundary ::segments)

(s/def ::path
  (s/and
   (s/keys :req-un [::segments] :opt-un [::style/style])
   #(core/connected? (:contents %))))

(s/def ::region
  (s/and (s/keys :req-un [::boundary] :opt-un [::style/style])
         #(core/closed-path? (:contents %))))

(s/def ::shape any?)

(s/def ::base-shape ::shape)

(s/def ::affine-transform
  (s/keys :req-un [::math/atx ::base-shape]))

(s/def ::frame
  (s/keys :req-un [::math/corner ::math/width ::math/height
                   ::contents]
          :opt-un [::style/style]))

(defmulti template-spec :type)

(s/def ::template
  #(satisfies? core/ITemplate %)
  #_(s/or :speced (s/multi-spec template-spec :type)
        :recursive (s/and
                    core/template?
                    #(s/valid? ::shape (core/expand-template %)))))

(s/def ::contents
  (s/coll-of ::shape :kind sequential?))

(s/def ::composite
  (s/or
   :explicit (s/keys :req-un [::contents] :opt-un [::style/style])
   :implicit (s/coll-of ::shape :kind sequential?)))

(s/def ::shape
  (s/or :template  ::template
        :segment   ::segment
        :path      ::path
        :region    ::region
        :frame     ::frame
        :composite ::composite
        :atx       ::affine-transform))

(ns lemonade.spec.style
  (:require [clojure.spec.alpha :as s]
            [lemonade.spec.math :as math]))

(def default-style
  {:stroke {:width 0
            :colour :black
            :dash []
            :ends :square
            :corners :mitre}
   :fill :none
   :opacity 1
   :font "sans serif 10px"})

(def rgba-re
  #"rgba\(\d{1,3}, *\d{0,3}, *\d{0,3}, *(\d\.\d+|\d)\)")

(s/def ::colour
  (s/or :keyword keyword?
        :hash (s/and string? #(re-matches #"\#[1234567890AaBbCcDdEeFf]{3,6}" %))
        :rgba (s/and string? #(re-matches rgba-re %))))

(s/def ::width ::math/non-negative)

(s/def ::dash
  (s/coll-of pos-int? :kind vector? :min-count 1))

(s/def ::corners keyword?)

(s/def ::stroke-map
  (s/keys :opt-un [::width ::colour ::dash ::corners]))

(s/def ::stroke
  (s/or :colour ::colour
        :map ::stroke-map))

(s/def ::stops
  (s/map-of pos-int? ::colour))

(s/def ::gradient
  (s/keys :req-un [::math/from ::math/to ::stops]))

(s/def ::fill
  (s/or :colour ::colour
        :colour-map (s/keys :req-un [::colour])
        :gradient ::gradient))

(s/def ::font string?)

(s/def ::opacity ::math/normalised)

(s/def ::style
  (s/keys :opt-un [::stroke ::fill ::font ::opacity]))

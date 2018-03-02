(ns ubik.spec.style
  (:require [clojure.spec.alpha :as s]
            [ubik.spec.math :as math]))

(def rgba-re
  #"rgba\(\d{1,3}, *\d{0,3}, *\d{0,3}, *(\d\.\d+|\d)\)")

(s/def ::stops
  (s/map-of pos-int? ::colour))

(s/def ::gradient
  (s/keys :req-un [::math/from ::math/to ::stops]))

(s/def ::colour
  (s/or :keyword keyword?
        :hash (s/and string? #(re-matches #"\#[1234567890AaBbCcDdEeFf]{3,6}" %))
        :rgba (s/and string? #(re-matches rgba-re %))
        :gradient ::gradient))

(s/def ::stroke ::colour)

(s/def ::fill ::colour)

(s/def ::font string?)

(s/def ::opacity ::math/normalised)

(s/def ::style
  (s/keys :opt-un [::stroke ::fill ::font ::opacity]))

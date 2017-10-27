(ns lemonade.spec.geometry
  #?(:cljs (:require-macros [lemonade.spec.geometry :refer [derive-spec]]))
  (:require [clojure.spec.alpha :as s]
            [lemonade.geometry :as geometry]))

#?(:clj
   (defmacro derive-spec
     "Shorthand for repeated derivations"
     {:style/indent 1}
     [spec & keys]
     `(do
        ~@(for [key keys]
            (list 's/def key spec)))))

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

(s/def ::angle ::scalar)

;;; Points 0d

(s/def ::point
  (s/coll-of ::scalar :kind sequential? :count 2))

(s/def ::points
  (s/coll-of ::point :kind sequential?))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef geometry/deg->rad
        :args (s/cat :angle ::real)
        :ret ::real)

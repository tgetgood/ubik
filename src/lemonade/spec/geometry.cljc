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

;; Non-degenerate
(s/def ::matrix
  (s/and (s/coll-of ::scalar :kind sequential? :count 4)
         #(> (apply geometry/det %) 0)))

(s/def ::translation ::vector)

(s/def ::atx
  (s/keys :req-un [::matrix ::translation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef lemonade.geometry/deg->rad
        :args (s/cat :angle ::real)
        :ret ::real)

(defn close-enough? [a b]
  (let [as (concat (:matrix a) (:translation a))
        bs (concat (:matrix b) (:translation b))]
    (->> (map - as bs)
         (map geometry/abs)
         (apply max)
         (> 1e-11))))

;; FIXME: This is exciting, I need higher precision arithmetic it seems. Need is
;; a bad word, this only comes up with huge translations. Well, 2000+ for 1e-12,
;; 32k for 1e-11. We're bleeding accuracy, so I suspect things will fail as you
;; zoom in.
(s/fdef lemonade.geometry/invert-atx
        :args (s/cat :atx ::atx)
        :ret ::atx
        :fn #(and
              (close-enough? geometry/id
                             (geometry/comp-atx (-> % :args :atx) (:ret %)))
              (close-enough? geometry/id
                             (geometry/comp-atx (:ret %) (-> % :args :atx)))))

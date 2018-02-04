(ns lemonade.spec.math
  #?(:cljs (:require-macros [lemonade.spec.math :refer [derive-spec]]))
  (:require [clojure.spec.alpha :as s]
            [lemonade.math :as math]))

#?(:clj
   (defmacro derive-spec
     "Shorthand for repeated derivations"
     {:style/indent 1}
     [spec & keys]
     `(do
        ~@(for [key keys]
            (list 's/def key spec)))))

;;; Scalars

(defn finite? [x]
  (and
   (not= x ##Inf)
   (not= x ##-Inf)))

;; Real numbers
(s/def ::real
  (s/and number?
         finite?
         (comp not math/nan?)))

(s/def ::scalar ::real)
(s/def ::non-negative (s/and ::scalar (comp not neg?)))
(s/def ::normalised (s/and ::scalar #(<= 0 % 1)))

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
         #(not (zero? (apply math/det %)))))

(s/def ::translation ::vector)

(s/def ::atx
  (s/keys :req-un [::matrix ::translation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef lemonade.math/deg->rad
        :args (s/cat :angle ::real)
        :ret ::real)

(defn close-enough? [a b]
  (let [as (concat (:matrix a) (:translation a))
        bs (concat (:matrix b) (:translation b))]
    (->> (map - as bs)
         (map math/abs)
         (apply max)
         (> 1e-11))))

;; FIXME: This is exciting, I need higher precision arithmetic it seems. Need is
;; a bad word, this only comes up with huge translations. Well, 2000+ for 1e-12,
;; 32k for 1e-11. We're bleeding accuracy, so I suspect things will fail as you
;; zoom in.
;; REVIEW: And they totally do, but the real reason is that we're reaching float
;; over/underflow conditions. The fact that webgl can't use doubles makes life a
;; little difficult.
(s/fdef lemonade.math/invert-atx
        :args (s/cat :atx ::atx)
        :ret ::atx
        :fn #(and
              (close-enough? math/id
                             (math/comp-atx (-> % :args :atx) (:ret %)))
              (close-enough? math/id
                             (math/comp-atx (:ret %) (-> % :args :atx)))))

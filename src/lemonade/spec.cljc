(ns lemonade.spec
  #?(:cljs (:require-macros [lemonade.spec :refer [derive-spec]]))
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check.generators]
            [clojure.pprint :refer [pprint pp]]))

;;;;; Helpers

(defn pe [spec]
  (->> spec
       s/exercise
       (map first)
       pprint))

;;;;; Styles

(s/def ::style
  (s/keys :req-un []))

;;;;; Geometry


(s/def ::on-screen
  (s/and ::real #(< 0 % 1200)))

(def nice-reals
  (let [nr (fn [] (s/gen ::on-screen))]
    {::real nr
     ::from nr
     ::to nr
     ::centre nr
     ::c1 nr
     ::c2 nr
     ::corner nr
     ::scalar nr
     ::non-negative nr}))

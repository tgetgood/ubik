(ns lemonade.spec-gen
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            clojure.test.check.generators
            [lemonade.geometry :as geometry]))

(s/def ::on-screen
  (s/and ::geometry/real #(< 0 % 1200)))

(def nice-reals
  (let [nr (fn [] (s/gen ::on-screen))]
    {::geometry/real nr}))

(defn connect
  "Given a seq of path segments, modify them so as to be connected.
  Intended for generation only."
  [acc paths]
  (if (empty? paths)
    acc
    (if (empty? acc)
      (recur (conj acc (first paths)) (rest paths))
      (let [end (-> acc last :to)]
        (recur (conj acc (assoc (first paths) :from end)) (rest paths))))))

(def square-gen
  (constantly
   (gen/fmap (fn [[c h]] {:corner c :height h :width h})
             (gen/tuple (s/gen ::geometry/point)
                        (s/gen ::geometry/non-negative)))))

(def segment-gen
  (fn []
    (gen/fmap #(connect [] %)
              (gen/fmap #(map first (s/exercise :lemonade.core/path-segment %))
                        (gen/int)))))

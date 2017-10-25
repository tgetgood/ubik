(ns lemonade.style
  (:require [clojure.spec.alpha :as s]))

(s/def ::style
  (s/keys :req-un []))

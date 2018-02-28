(ns lemonade.hosts.jvm-quil
  (:require [lemonade.core :as core]))

(defn ne! []
  (throw (Exception. "Not Implemented!")))

(defn host []
  (reify
    core/Host

    (event-system [_] (ne!))
    (render-fn [_] (ne!))
    (width [_] (ne!))
    (height [_] (ne!))
    (resize-frame [_ [width height]] (ne!))
    (fullscreen [_] (ne!))))

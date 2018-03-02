(ns ubik.hosts.jvm-quil
  (:require [ubik.core :as core]))

(defn ne! []
  (throw (Exception. "Not Implemented!")))

(defn host [opts]
  (reify
    core/Host

    (setup [_] (ne!))
    (teardown [_] (ne!))
    (render-fn [_] (ne!))
    (width [_] (ne!))
    (height [_] (ne!))
    (fullscreen [_] (ne!))))

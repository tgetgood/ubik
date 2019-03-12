(ns ubik.core
  (:require [clojure.core.async :as async :include-macros true]
            [falloleen.jfx :as fx]
            [ubik.events :as events]
            [ubik.codebase :as codebase]
            [ubik.subs :as subs :include-macros true]
            [ubik.process :as process :include-macros true]
            [ubik.rt :as rt])
  (:import javafx.scene.control.TextArea))

(defonce stages (atom {}))

(defn create-code-stage [branch sym]
  (let [k (str branch "-" sym)]
    (if (contains? @stages k)
      (@stages k)
      (let [p @(fx/code-stage)
            ev-map (events/bind-text-area! (:area p))
            res {:node (:area p) :stage (:stage p) :event-streams ev-map}]
        (swap! stages assoc k res)
        res))))

(defn lift [f]
  {:in (fn [_ x]
         (let [out (f x)]
           (when out
             {:emit out})))})

(defn text-renderer [^TextArea node]
  (fn [text]
    (fx/fx-thread
     (let [caret (.getCaretPosition node)]
       (.setText node text)
       (.positionCaret node caret)))))

(def image-signal codebase/image-signal)

(def source-effector codebase/source-effector)

(ns ubik.core
  (:require [clojure.core.async :as async :include-macros true]
            [falloleen.jfx :as fx]
            [ubik.events :as events]
            [ubik.codebase :as codebase]
            [ubik.subs :as subs :include-macros true]
            [ubik.process :as process :include-macros true]
            [ubik.rt :as rt])
  (:import javafx.scene.control.TextArea))

(defn create-code-stage []
  (let [p @(fx/code-stage)
        ev-map (events/bind-text-area! (:area p))]
    {:node (:area p) :stage (:stage p) :event-streams ev-map}))

(defn lift [f]
  {:input (fn [_ x] (f x))})

(defn text-renderer [^TextArea node]
  (fn [text]
    (fx/fx-thread
     (let [caret (.getCaretPosition node)]
       (.setText node text)
       (.positionCaret node caret)))))

(def image-signal codebase/image-signal)

(def source-effector codebase/source-effector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Aggregated API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Subscriptions

;; Distinction: subscriptions are reactive, signals are active. This is more
;; important than it may seem.

(defn wire [signal xform])

(defn pipe [& xforms])

(defmacro transducer
  ([bindings body-map]
   `(transducer {} ~bindings ~body-map))
  ([opts bindings body-map]
   (let [m (cond-> opts (= 2 (count bindings)) (assoc :state true))]
     (println m)
     (into {} (map (fn [[k v]] [k `(with-meta (fn ~bindings ~v) ~m)])
                   body-map)))))

(defmacro deft [n & args]
  (let [doc  (if (string? (first args)) (first args) nil)
        args (if doc (rest args) args)]
    `(def ~n
       ~@(when doc [doc])
       (transducer ~@args))))

(defmacro subscription
  "Given a form which derefs other subscriptions returns a new subscription
  that reacts to its dependencies. If form does not depend on any subscriptions
  then it is evaluated and it's (static) value returned."
  [form]
  `(subs/subscription ~form))

;;; Processes

(defmacro emit [& args]
  `(process/emit ~@args))

(def stateful-process process/stateful-process)

(def subscription? subs/subscription?)

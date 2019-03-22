(ns ubik.codebase.builtin
  (:require [falloleen.jfx :as fx]
            [ubik.events :as events]
            [ubik.process :as process])
  (:import javafx.scene.control.TextArea))

(def stages (atom {}))

(defn create-code-stage [k]
  (if (contains? @stages k)
    (@stages k)
    (let [p @(fx/code-stage)
          ev-map (events/bind-text-area! (:area p))
          res {:node (:area p) :stage (:stage p) :event-streams ev-map}]
      (swap! stages assoc k res)
      res)))

;; FIXME: This jfx specific code does not belong in Ubik. But it also doesn't
;; belong in Falloleen. It's a kludge anyway so why worry about factoring?
(defn text-renderer [^TextArea node]
  (fn [text]
    (fx/fx-thread
     (let [caret (.getCaretPosition node)]
       (.setText node text)
       (.positionCaret node caret)))))

(defn topo-effector [t]
  (println "topo-effector!")
  (println t)
  #_(let [k (keyword (gensym))]
    (topo/init-topology! k t)))

(def make-node process/make-node)

(def signal process/signal)

(defonce image-signal
  (signal ::image-signal))

(defn source-effector [sym]
  (fn [form]
    (process/send image-signal {"stm" form})))

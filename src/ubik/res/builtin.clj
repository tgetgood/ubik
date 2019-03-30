(ns ubik.res.builtin
  (:require [clojure.datafy :refer [datafy]]
            [falloleen.jfx :as fx]
            [ubik.codebase :as codebase]
            [ubik.events :as events]
            [ubik.process :as process]
            [ubik.topology :as topo]
            [ubik.util :as util])
  (:import javafx.scene.control.TextArea))

(def stages (atom {}))

(defn create-code-stage [k]
  (if (contains? @stages k)
    (@stages k)
    (let [p @(fx/code-stage)
          ev-map (events/bind-text-area! k (:area p))
          res {:node (:area p) :stage (:stage p) :event-streams ev-map}]
      (swap! stages assoc k res)
      res)))

;; FIXME: This jfx specific code does not belong in Ubik. But it also doesn't
;; belong in Falloleen. It's a kludge anyway so why worry about factoring?
(defn text-renderer [^TextArea node]
  (with-meta
    (fn [text]
      (fx/fx-thread
       (let [caret (.getCaretPosition node)]
         (.setText node text)
         (.positionCaret node caret))))
    {:ref [:builtin 'text-renderer]}))

(def topo-effector
  (with-meta (fn [t]
               (topo/init-topology! t))
    {:ref [:builtin 'topo-effector]}))

(def make-node process/make-node)
(def signal process/signal)

(def effector process/effector)

(defmacro process [name nodefn]
  (let [s (if (sequential? nodefn) (last nodefn) nodefn)]
    `(process/process ~name (with-meta ~nodefn (meta ~s)))))

(defn source-effector [sym]
  (with-meta
    (fn [form]
      (util/log :debug {:name "source effector"
                        :form form})
      (try
        (let [snippet (eval form)
              sha (:sha1 (meta snippet))]
          (codebase/commit sym sha))
        (catch Exception e (util/log :error {:name "source effector"
                                             :exception (datafy e)}))))
    {:ref [:builtin 'source-effector]
     :arg sym}))

(def edit codebase/edit)

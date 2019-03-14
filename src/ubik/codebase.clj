(ns ubik.codebase
  (:require [clojure.core.async :as async]
            [clojure.datafy :refer [datafy]]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Branching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def you
  "Possibly frivolous test of self control: Can I avoid refering to anyone as a
  'user' for an entire project?"
  (System/getProperty "user.name"))

(def machine
  (-> (Runtime/getRuntime)
      (.exec "hostname")
      .getInputStream
      slurp
      string/trim))

(defonce ^:private branch-stem
  (atom "master"))

;; TODO: The actual act of branching...
(defn current-branch []
  (str you "/" machine "/" @branch-stem))

(def core-ns
  "Namespace into which all of the builtin bootstrapping code is loaded."
  "editor.core")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code as data in a database.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; External API

(def image-signal-in
  (async/chan))

(def image-signal
  (let [ch (async/chan)]
    #_(async/go-loop []
      (when-let [msg (async/<! image-signal-in)]
        (try
          (let [ns (pull-ns (current-branch) core-ns)
                ns-map (into {} (map (fn [[k v]] [k (pull-snip v)])) ns)]
            (async/>! ch ns-map))
          (catch Exception e (println (datafy e))))
        (recur)))
    ch))

(defn source-effector [branch sym]
  (fn [form]
    (async/put! image-signal-in true)))

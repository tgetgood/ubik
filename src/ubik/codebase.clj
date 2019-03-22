(ns ubik.codebase
  (:refer-clojure :exclude [intern])
  (:require [clojure.string :as string]
            [ubik.codebase.config :as config :refer [*store*]]
            [ubik.codebase.storage :as store]))

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


(defn ns-sym [sym id]
  {:ns/symbol sym :id id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets
;;
;; Snippets are minimal, meaningful, fragments of code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn intern [store snip]
  (store/intern store snip))

(defn create-snippet
  "Expects a map with keys :form and :links. Returns the stored snippet matching
  the given code, creating a new entry if necessary."
  [snip]
  (with-meta
    (if-let [snip (store/by-value *store* snip)]
      snip
      (intern *store* (assoc snip :id (java.util.UUID/randomUUID))))
    {::snippet true}))

(defmacro snippet
  "Syntactic sugar for writing linked snippets."
  {:style/indent [1]}
  [bindings expr]
  `(create-snippet {:form  '~expr
                    :links '~bindings}))

(defn edit
  "Returns snippet in easily editable form by id."
  [id]
  (let [{:keys [form links]} (store/lookup *store* id)]
    `(snippet ~links
       ~form)))

;;;;; External API

(defn internal-ns-map []
  (store/as-map config/*branch*))

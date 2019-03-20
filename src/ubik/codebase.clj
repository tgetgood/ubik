(ns ubik.codebase
  (:require [clojure.string :as string]
            [ubik.rt :as rt]
            [ubik.storage :as store]))

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

(def master-uri
  "Temp uri of master branch"
  "master.db")

(def snippet-db-uri
  "Just a file at the moment."
  "residential.db")

(defonce
  ^{:dynamic true
    :doc "Current branch. Not that branching is supported robustly at present."}
  *branch*
  (store/branch master-uri))

(defn ns-sym [sym id]
  {:ns/symbol sym :id id})

(def ^:dynamic *store*
  "Default code storage backend."
  (store/file-backed-mem-store snippet-db-uri))

(def
  ^{:doc     "The namespace into which all interned code gets loaded."
    :dynamic true}
  *primary-ns*
  (let [ns-name 'codebase.internal]
    (if-let [ns (find-ns ns-name)]
      ns
      (let [ns (create-ns ns-name)]
        (binding [*ns* ns]
          (require '[falloleen.core :as falloleen]
                   '[clojure.pprint]
                   '[ubik.core :refer :all]
                   'ubik.rt)
          (import '[ubik.rt Signal MProcess BasicSignal Multiplexer]))
        ns))))

(defn interned-var-name [id]
  (symbol (str 'f$ id)))

(defn qualified-var-name [ns id]
  (symbol (name (ns-name ns))
          (name (interned-var-name id))))

(defn gen-code-for-body
  ([body] (gen-code-for-body *primary-ns* body))
  ([refer-ns {:keys [form links]}]
   `(let [~@(mapcat (fn [[n id]]
                      (let [v (qualified-var-name refer-ns id)]
                        `[~n ~v]))
                    links)]
      ~form)))

(defn load-ns
  "Load all code snippets into ns. Each snippet becomes a var named by its
  id. Links are captured as lexical references to other vars in the same ns."
  ([var-map] (load-ns *primary-ns* var-map))
  ([ns var-map]
   (let [current-map (ns-interns ns)
         new-snippets (remove #(contains? current-map (key %)) var-map)]
     (doseq [[id body] new-snippets]
       (clojure.core/intern
        ns
        (with-meta (interned-var-name id) body)
        (eval (gen-code-for-body ns body)))))))

(defn clear-ns [ns]
  (let [vars (keys (ns-interns ns))]
    (run! #(ns-unmap ns %) vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets
;;
;; Snippets are minimal, meaningful, fragments of code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-snippet
  "Expects a map with keys :form and :links. Returns the stored snippet matching
  the given code, creating a new entry if necessary."
  [snip]
  (with-meta
    (if-let [snip ( *store* snip)]
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

(def image-signal
  (rt/signal ::image-signal))

(defn source-effector [branch sym]
  (fn [form]
    (rt/send image-signal {"stm" form})))

;;;; Extras

(defmacro ref-sig
  "Returns a signal which will emit the new version of ref when any of its
  components or dependencies change."
  {:style/indent [1]}
  [deps body])

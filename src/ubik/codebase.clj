(ns ubik.codebase
  (:refer-clojure :exclude [intern])
  (:require [clojure.string :as string]
            [ubik.codebase.config :as config]
            [ubik.process :as process]
            [ubik.codebase.storage :as store]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Branching
;;;;;
;;;;; Currently there is only one branch, which is to say no branching. That's a
;;;;; problem. but far from my most pressing.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn advance-branch [[sym ref]]
  (store/intern config/*branch* (store/ns-sym sym ref)))

(defn populate-nses [m]
  (run! advance-branch m))

(defn current-ns-map
  "Returns the ns-map of the current branch. The ns map is a map whose keys are
  namespace names (strings) and whose values are maps from var names (again
  strings) to ns entries."
  []
  (store/as-map config/*branch*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Working with codebase images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce image-signal
  (let [sig (process/signal ::image)]
    (process/send sig (current-ns-map))
    sig))

(defn commit
  "Update current branch so that sym points to id."
  [sym sha]
  (store/intern config/*branch* (store/ns-sym sym sha))
  (process/send-new image-signal (current-ns-map)))

(defn lookup
  "Somewhat ad hoc 'lookup thing' fn. ¿DWIM at it's finest?"
  ([x]
   (store/lookup
    ;; FIXME: This looks a lot like lazy polymorphism
    (cond
      (string? x)  config/*code*
      (keyword? x) config/*branch*)
    x))
  ([sym time]
   (store/branch-lookup config/*branch* sym time)))

(defn codebase
  "Returns a map of all snippets ever created indexed by id."
  []
  (store/as-map config/*code*))

(defn resolve-links
  "Links must always point to concrete SHAs. If a link is given as an ns
  qualified symbol, then look up the sha that that symbol points to right now."
  ;; REVIEW: Very convenient for bootstrapping, but requires multiple loads to
  ;; get right. Is it worth it long term?
  [m]
  (into {}
        (map (fn [[k v]]
               [k (if (string? v)
                    v
                    (:ref (lookup v)))])
             m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets
;;
;; Snippets are minimal, meaningful, fragments of code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-snippet
  "Expects a map with keys :form and :links. Returns the stored snippet matching
  the given code, creating a new entry if necessary."
  [snip]
  (store/intern config/*code* snip))

(defmacro snippet
  "Syntactic sugar for writing linked snippets."
  {:style/indent [1]}
  [bindings expr]
  `(create-snippet {:form  '~expr
                    :links (resolve-links '~bindings)}))

(defn edit
  "Returns code of snippet in a form, which when edited and evalled, will create
  a new snippet."
  [id]
  (let [{:keys [form links]} (store/lookup config/*code* id)]
    `(snippet ~links
       ~form)))

(def ^:dynamic *update-branch* false)

(defmacro sdef
  ([n s]
   `(sdef ~n "" ~s))
  ([n doc s]
  `(do
     (def ~n ~doc ~s)
     (when *update-branch*
       (advance-branch [~(keyword "core" (name n)) (:sha1 (meta ~n)) ])))))

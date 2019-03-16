(ns ubik.codebase
  (:refer-clojure :exclude [intern])
  (:require [clojure.core.async :as async]
            [clojure.datafy :refer [datafy]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [ubik.rt :as rt]))

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
;; Code Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def persistence-uri
  "Just a file at the moment."
  "residential.db")

(defprotocol Store
  (intern [this snippet] "Intern a snippet in the code store")
  (retrieve [this id] "Retrieves a code snippet by id.")
  (lookup [this snip] "Returns the snip if it is in the store, nil otherwise.")
  (as-map [this] "Retrieves the entire store as a map from ids to snippets."))

(defrecord FileStore [file-name]
  ;; This will fall over very easily. We need indicies. Hell, we need a database.
  Store
  (intern [_ snippet]
    (assert (contains? snippet :id)
            "You're trying to intern a code fragment without an id. You may as
            well drop it on the floor to its face.")
    (spit file-name (str snippet "\n") :append true)
    snippet)
  (retrieve [_ id]
    (with-open [rdr (io/reader file-name)]
      (->> rdr
           line-seq
           (map read-string)
           (filter #(= (:id %) id))
           first)))
  (lookup [_ snip]
    (let [snip (dissoc snip :id)]
      (with-open [rdr (io/reader file-name)]
        (->> rdr
             line-seq
             (map read-string)
             (filter #(= snip (dissoc % :id)))
             first))))
  (as-map [_]
    (with-open [rdr (io/reader file-name)]
      (into {}
            (comp (map read-string)
                  (map (fn [x] [(:id x) x])))
            (line-seq rdr)))))

(def ^:dynamic *store*
  "Default code storage backend."
  (FileStore. persistence-uri))

(defonce
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
                   '[ubik.core :refer :all]))
        ns))))

(defn interned-var-name [id]
  (symbol (str 'f$ id)))

(defn gen-code-for-body
  ([body] (gen-code-for-body *primary-ns* body))
  ([refer-ns {:keys [form links]}]
   `(let [~@(mapcat (fn [[n id]]
                      (let [v (symbol (name (ns-name refer-ns))
                                      (name (interned-var-name id)))]
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
    (if-let [snip (lookup *store* snip)]
      snip
      (intern *store* (assoc snip :id (java.util.UUID/randomUUID))))
    {::snippet true}))

(defmacro snippet
  "Syntactic sugar for writing linked snippets."
  {:style/indent [1]}
  [bindings expr]
  `(create-snippet {:form  '~expr
                    :links '~bindings}))

(defn s1 [snips links bindings]
  (if (seq bindings)
    (if (uuid? (second bindings))
       (s1 snips (assoc links (first bindings) (second bindings))
              (rest (rest bindings)))
       (let [snip (create-snippet {:links links
                                   :form (second bindings)})]
         (s1 (conj snips snip) (assoc links (first bindings) (:id snip))
             (rest (rest bindings)))))
    snips))

(defmacro quote-all [syms]
 (if (seq syms)
    `(into ['~(first syms)]
           (quote-all ~(rest syms)))) )

(defmacro snippets
  {:style/indent :let}
  [& bindings]
  `(s1 [] {} (quote-all ~bindings)))

(defn edit
  "Returns snippet in easily editable form by id."
  [id]
  (let [{:keys [form links]} (retrieve *store* id)]
    `(snippet ~links
       ~form)))

;;;;; External API

(def image-signal-in
  (async/chan))

(def image-signal
  (rt/signal))

(defn source-effector [branch sym]
  (fn [form]
    (async/put! image-signal-in true)))

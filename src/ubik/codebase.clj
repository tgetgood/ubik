(ns ubik.codebase
  (:refer-clojure :exclude [intern])
  (:require [clojure.core.async :as async]
            [clojure.datafy :refer [datafy]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [ubik.rt :as rt]))

(def image-signal
  (rt/signal ::image-signal))

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
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def master-uri
  "Temp uri of master branch"
  "master.db")

(def snippet-db-uri
  "Just a file at the moment."
  "residential.db")

(defn now []
  (java.util.Date.))

(defn append-line
  "Appends (str x \"\\n\") to file."
  [filename x]
  (spit filename (str x "\n") :append true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Store
  (intern [this snippet] "Intern a snippet in the code store")
  (retrieve [this id] "Retrieves a code snippet by id."))

(defprotocol ReverseLookup
   (lookup [this snip] "Returns the snip if it is in the store, nil otherwise."))

(defprotocol ValueMap
   (as-map [this] "Retrieves the entire store as a map from ids to snippets."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord FileBackedBranch [filename]
  Store
  (intern [this entry]
    (let [new (assoc entry :op :add :time (now))]
      (when-let [old (retrieve this entry)]
        (append-line filename (assoc old :op :retract)))
      (append-line filename new )))
  (retrieve [this sym]
    (with-open [rdr (io/reader filename)]
      (->> rdr
           line-seq
           (map read-string)
           (filter #(= sym (get % :ns/symbol)))
           first)))

  ValueMap
  (as-map [this]
    (with-open [rdr (io/reader filename)]
      (reduce (fn [nses s]
                (let [m (read-string s)
                      op (:op m)
                      ns (namespace (:ns/symbol m))
                      sym (name (:ns/symbol m))]
                  (if (= op :add)
                    (assoc-in nses [ns sym] m)
                    (update nses ns dissoc sym))))
              {}
              (line-seq rdr) ))))

(defonce
  ^{:dynamic true
    :doc "Current branch. Not that branching is supported robustly at present."}
  *branch*
  (FileBackedBranch. master-uri))

(defn ns-sym [sym id]
  {:ns/symbol sym :id id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Persistence (snippets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord FileStore [filename]
  ;; This will fall over very easily. We need indicies. Hell, we need a database.
  Store
  (intern [_ snippet]
    (assert (contains? snippet :id)
            "You're trying to intern a code fragment without an id. You may as
            well drop it on the floor to its face.")
    (let [snippet (assoc snippet :time (now))]
      (append-line filename snippet)
      snippet))
  (retrieve [_ id]
    (with-open [rdr (io/reader filename)]
      (->> rdr
           line-seq
           (map read-string)
           (filter #(= (:id %) id))
           first)))

  ReverseLookup
  (lookup [_ snip]
    (let [snip (dissoc snip :id :time)]
      (with-open [rdr (io/reader filename)]
        (->> rdr
             line-seq
             (map read-string)
             (filter #(= snip (dissoc % :id :time)))
             first))))

  ValueMap
  (as-map [_]
    (with-open [rdr (io/reader filename)]
      (into {}
            (comp (map read-string)
                  (map (fn [x] [(:id x) x])))
            (line-seq rdr)))))

(defn file-backed-mem-store [filename]
  (let [store (FileStore. filename)
        cache (atom (as-map store))]
    (reify Store
      (intern [_ snippet]
        (let [snippet (intern store snippet)]
          (swap! cache assoc (:id snippet) snippet))
        ;; TODO: Decouple this somehow. Eventually. We can't go and put all of
        ;; the code ever defined into a message every time a char is
        ;; typed. Though it is just a reference, so maybe we can...'
        (rt/send image-signal @cache))
      (retrieve [_ id]
        (get @cache id))

      ReverseLookup
      (lookup [_ snip]
        ;; TODO: Indicies
        (let [snip (dissoc snip :id :time)]
          (first (filter #(= snip (dissoc % :id :time)) (vals @cache)))))

      ValueMap
      (as-map [_]
        @cache))))

(def ^:dynamic *store*
  "Default code storage backend."
  (file-backed-mem-store snippet-db-uri))

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

(defn source-effector [branch sym]
  (fn [form]
    (rt/send image-signal {"stm" form})))

;;;; Extras

(defmacro ref-sig
  "Returns a signal which will emit the new version of ref when any of its
  components or dependencies change."
  [deps body])

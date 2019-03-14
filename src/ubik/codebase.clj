(ns ubik.codebase
  (:refer-clojure :exclude [intern])
  (:require [clojure.core.async :as async]
            [clojure.datafy :refer [datafy]]
            [clojure.java.io :as io]
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
;; Codebase without a file tree (but still with files for the time being).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro snippet
  "Syntactic sugar for writing linked snippets."
  {:style/indent [1]}
  [bindings expr]
  `{:form  '~expr
    :links '~bindings
    :id    (java.util.UUID/randomUUID)})

(def persistence-uri
  "Just a file at the moment."
  "residential.db")

(defprotocol Store
  (intern [this snippet] "Intern a snippet in the code store")
  (retrieve [this id]
    "Retrieves a code snippet by id. N.B.: This isn't always efficient.")
  (as-map [this] "Retrieves the entire store as a map from ids to snippets."))

(defrecord FileStore [file-name]
  Store
  (intern [_ snippet]
    (assert (contains? snippet :id)
            "You're trying to intern a code fragment without an id. You may as
            well drop it on the floor to its face.")
    (spit file-name (str snippet "\n") :append true)
    (:id snippet))
  (retrieve [_ id]
    (with-open [rdr (io/reader file-name)]
      (->> rdr
           line-seq
           (map read-string)
           (filter #(= (:id %) id))
           first)))
  (as-map [_]
    (with-open [rdr (io/reader file-name)]
      (into {}
            (comp (map read-string)
                  (map (fn [x] [(:id x) x])))
            (line-seq rdr)))))

(defonce store
  (FileStore. persistence-uri))

(defn edit [store id]
  (let [{:keys [form links]} (retrieve store id)]
    `(snippet ~links
       ~form)))

(defonce
  ^{:doc "The namespace into which all interned code gets loaded."}
  primary-ns
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

(defn gen-code-for-body [refer-ns {:keys [form links]}]
  `(let [~@(mapcat (fn [[n id]]
                     (let [v (symbol (name (ns-name refer-ns))
                                     (name (interned-var-name id)))]
                       `[~n ~v]))
                   links)]
     ~form))

(defn ns-intern-all [ns var-map]
  (let [current-map (ns-interns ns)
        new-snippets (remove #(contains? current-map (key %)) var-map)]
    (doseq [[id body] new-snippets]
      (clojure.core/intern
       ns
       (with-meta (interned-var-name id) body)
       (eval (gen-code-for-body ns body))))))

(defn clear-ns [ns]
  (let [vars (keys (ns-interns ns))]
    (run! #(ns-unmap ns %) vars)))

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

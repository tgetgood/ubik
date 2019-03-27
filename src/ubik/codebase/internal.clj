(ns ubik.codebase.internal
  (:require [clojure.string :as string]
            [falloleen.core :as falloleen]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [ubik.codebase.builtin :refer :all]
            [ubik.codebase.config :as config]
            [ubik.codebase.storage :as store]))

(def internal (the-ns 'ubik.codebase.internal))

(defn interned-var-name [id]
  (symbol (str 'f$ id)))

(defn full-var-name [id]
  (symbol (name (ns-name internal))
          (name (interned-var-name id))))

(defn id-var [id]
  (get (ns-interns internal) (interned-var-name id)))

(defn declare-all []
  (let [ks (keys (store/as-map config/*store*))]
    (run! #(intern internal %)
         (map interned-var-name ks))))

(defn ns-link? [link]
  (and (keyword? (:ref link)) (inst? (:time link))))

(defn lookup-link [link]
  (when-let [ref (store/branch-lookup config/*branch* (:ref link) (:time link))]
    (full-var-name (:ref ref))))

(defn gen-ref [link]
  (cond
    (string? link)  (full-var-name link)
    (ns-link? link) (lookup-link link)))

(defn gen-code-for-body
  [{:keys [form links] :as bid}]
  `(let [~@(mapcat (fn [[n id]]
                     (let [v (gen-ref id)]
                       (when (nil? v)
                         (log/error "Broken link:" id))
                       `[~n (if (delay? ~v) @~v ~v)]))
                   links)]
     (do (log/debug "Invoking:" '~form "(" ~(:sha1 (meta bid)) ")")
         ~form)))

(defn gen-code-for-id [id]
  (gen-code-for-body (store/lookup config/*store* id)))

(defn clear-ns
  []
  (let [vars (filter #(string/starts-with? (name (symbol %)) "f$")
                     (keys (ns-interns internal)))]
    (run! #(ns-unmap internal %) vars)))

(defn load-ns
  "Load all code snippets into ns. Each snippet becomes a var named by its
  id. Links are captured as lexical references to other vars in the same ns."
  []
  (declare-all)
  (let [m (store/as-map config/*store*)
        ns (ns-interns internal)]
    (doseq [[id body] m]
      (let [v (get ns (interned-var-name id))
            form (gen-code-for-body body)]
        (when-not (= body (::body (meta v)))
          (log/trace "Defining" v "for first time.")
          (alter-meta! v assoc ::body body)
          (alter-var-root v (constantly
                             (delay
                              (binding [*ns* internal]
                                (eval form))))))))))

(defn invoke-by-id
  "Given the id of a snippet, return the evaluated form to which it refers."
  [id]
  ;;REVIEW: This seems a little excessive. Do you have a better idea?
  (load-ns)
  @@(id-var id))

(defn invoke-head
  "Returns the evaluated form pointed to by sym at the head of the current
  branch."
  [sym]
  (let [link (store/lookup config/*branch* sym)]
    (invoke-by-id (:ref link))))

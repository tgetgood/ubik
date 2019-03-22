(ns ubik.codebase.internal
  (:require [clojure.string :as string]
            [falloleen.core :as falloleen]
            [clojure.pprint]
            [taoensso.timbre :as log]
            [ubik.codebase.builtin :refer :all]
            [ubik.codebase.config :as config]
            [ubik.codebase.storage :as store])
  (:import [ubik.rt Signal MProcess BasicSignal Multiplexer]))

(def internal (the-ns 'ubik.codebase.internal))

(defn interned-var-name [id]
  (symbol (str 'f$ id)))

(defn full-var-name [id]
  (symbol (name (ns-name internal))
          (name (interned-var-name id))))

(defn id-var [id]
  (get (ns-interns internal) (interned-var-name id)))

(defn invoke-by-id
  "Given the id of a snippet, return the evaluated form to which it refers."
  [id]
  @@(id-var id))

(defn declare-all []
  (let [ks (keys (store/as-map config/*store*))]
    (run! #(intern internal %)
         (map interned-var-name ks))))

(defn gen-code-for-body
  [{:keys [form links]}]
  `(let [~@(mapcat (fn [[n id]]
                     (let [v (full-var-name id)]
                       `[~n (deref ~v)]))
                   links)]
     ~form))

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
          (alter-var-root v (constantly (delay (eval form)))))))))

(load-ns)

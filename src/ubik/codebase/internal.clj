(ns ubik.codebase.internal
  (:require [clojure.string :as string]
            [falloleen.core :as falloleen]
            [clojure.pprint]
            [taoensso.timbre :as log]
            ubik.rt
            [ubik.codebase.core :as core]
            [ubik.codebase.storage :as store])
  (:import [ubik.rt Signal MProcess BasicSignal Multiplexer]))

(defn interned-var-name [id]
  (symbol (str 'f$ id)))

(defmacro declare-all []
  (let [ks (keys (store/as-map core/*store*))
        ns (ns-interns *ns*)]
    `(do
       ~@(map
          (fn [s]
            (let [v (interned-var-name s)]
              (when-not (contains? ns v)
                `(declare ~v))))
          ks))))

(declare-all)

(defn gen-code-for-body
  [{:keys [form links]}]
  `(let [~@(mapcat (fn [[n id]]
                     (let [v (interned-var-name id)]
                       `[~n (deref ~v)]))
                   links)]
     ~form))

(defn clear-ns
  []
  (let [vars (filter #(string/starts-with? (name (symbol %)) "f$")
                     (keys (ns-interns *ns*)))]
    (run! #(ns-unmap *ns* %) vars)))

(defn load-ns
  "Load all code snippets into ns. Each snippet becomes a var named by its
  id. Links are captured as lexical references to other vars in the same ns."
  []
  (let [m (store/as-map core/*store*)
        ns (ns-interns *ns*)]
    (doseq [[id body] m]
      (let [v (get ns (interned-var-name id))
            form (gen-code-for-body body)]
        (when-not (= body (::body (meta v)))
          (log/trace "Defining" v "for first time.")
          (alter-meta! v assoc ::body body)
          (alter-var-root v (constantly (delay (eval form)))))))))

(load-ns)

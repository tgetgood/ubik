(ns ubik.res.code-gen
  (:require [clojure.string :as string]
            [falloleen.core :as falloleen]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as log]
            [ubik.codebase :as codebase]
            [ubik.res.builtin :refer :all]))

(def internal
  "Reference to this namespace itself."
  (the-ns 'ubik.res.code-gen))

(defn interned-var-name [id]
  (symbol (str 'f$ id)))

(defn full-var-name [id]
  (symbol (name (ns-name internal))
          (name (interned-var-name id))))

(defn id-var [id]
  (get (ns-interns internal) (interned-var-name id)))

(defn declare-all []
  (let [ks (keys (codebase/codebase))]
    (run! #(intern internal %)
         (map interned-var-name ks))))

(defn ns-link? [link]
  (and (keyword? (:ref link)) (inst? (:time link))))

(defn lookup-link [link]
  (when-let [ref (codebase/lookup (:ref link) (:time link))]
    (full-var-name (:ref ref))))

(defn gen-ref [link]
  (cond
    (string? link)  (full-var-name link)
    (ns-link? link) (lookup-link link)))

(defn gen-code-for-body
  [{:keys [form links]}]
  `(let [~@(mapcat (fn [[n id]]
                     (let [v (gen-ref id)]
                       (when (nil? v)
                         (log/error "Broken link:" id))
                       `[~n (force ~v)]))
                   links)]
     ~form))

(defn gen-code-for-id [id]
  (gen-code-for-body (codebase/lookup id)))

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
  (let [m (codebase/codebase)
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

(defn reload!
  "Wipes and reloads all dynamic (resident) code."
  []
  (clear-ns)
  (load-ns))

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
  (let [link (codebase/lookup sym)]
    (invoke-by-id (:ref link))))

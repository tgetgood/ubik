(ns ubik.res.code-gen
  (:require [clojure.string :as string]
            [falloleen.core :as falloleen]
            [clojure.pprint :refer [pprint]]
            [ubik.codebase :as codebase]
            [ubik.res.builtin :refer :all]
            [ubik.util :as util]))

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

(defn gen-ref [link]
  {:pre [(string? link)]}
  (with-meta (full-var-name link)
    {:id link}))

(defn gen-code-for-body
  [{:keys [form links]}]
  `(let [~@(mapcat (fn [[n id]]
                     (let [v (gen-ref id)]
                       (when (nil? v)
                         (util/log :error "Broken link:" id))
                       `[~n (with-meta (force ~v) ~(meta v))]))
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
          (util/log :trace "Defining" v "for first time.")
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
  (let [v @@(id-var id)]
    (if (instance? clojure.lang.IMeta v)
      (with-meta v {:id id})
      v)))

(defn invoke-head
  "Returns the evaluated form pointed to by sym at the head of the current
  branch."
  [sym]
  (let [link (codebase/lookup sym)]
    (invoke-by-id (:ref link))))

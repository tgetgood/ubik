(ns ubik.codebase
  (:refer-clojure :exclude [find-ns create-ns])
  (:require [clojure.core.async :as async]
            [clojure.datafy :refer [datafy]]
            [clojure.string :as string]
            [datomic.api :as d]
            [ubik.db :refer [push pull conn]]))

;;;;; Branching

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

(def core-ns
  "Namespace into which all of the builtin bootstrapping code is loaded."
  "editor.core")

;; TODO: The actual act of branching...
(defn current-branch []
  (str you "/" machine "/" @branch-stem))

;;;;; Static code processing

(defn build-snip [sym code]
  (let [id (push code)
        ent {:db/id "snip"
             :snippet/form id
             :snippet/name (name sym)}
        ent-id (get-in @(d/transact conn [ent])
                       [:tempids "snip"])]
    ent-id))

(defn find-ns
  "Returns entity id of namespace in the given branch."
  [branch ns-name]
  (d/q '[:find ?ns .
         :in $ ?branch ?ns-name
         :where
         [?b :branch/name ?branch]
         [?b :branch/namespace ?ns]
         [?ns :namespace/name ?ns-name]]
       (d/db conn) branch ns-name))

(defn ns-retraction [branch ns-name]
  (let [eid (find-ns branch ns-name)]
    (when eid
      [[:db/retract [:branch/name branch] :branch/namespace eid]])))

(def empty-map
  (push {}))

(defn intern-ns [branch ns-name ns-map]
  (get-in @(d/transact conn (into (ns-retraction branch ns-name)
                                  [{:db/id          "new-ns"
                                    :namespace/name ns-name
                                    :namespace/map  ns-map}
                                   {:branch/name      branch
                                    :branch/namespace "new-ns"}]))
          [:tempids "new-ns"]))

;;;;; Retrieval

(defn find-ns-map [branch ns-name]
  (d/q '[:find ?ns-map .
          :in $ ?branch ?ns-name
          :where
          [?b :branch/name ?branch]
          [?b :branch/namespace ?ns]
          [?ns :namespace/name ?ns-name]
          [?ns :namespace/map ?ns-map]]
        (d/db conn) branch ns-name))

(defn pull-ns [branch ns-name]
  (when-not (find-ns branch ns-name)
    (intern-ns branch ns-name empty-map))
  (pull (find-ns-map branch ns-name)))

(defn intern-code
  "Upserts var into namespace."
  [branch sym form]
  (let [old-ns (pull-ns branch (namespace sym))
        _ (println old-ns)
        snip  (build-snip sym form)
        new-ns (assoc old-ns (name sym) snip)]
    (println new-ns)
    (intern-ns (current-branch) (namespace sym) (push new-ns))))

(defn pull-snip [snip-id]
  (let [bits (d/pull
              (d/db conn)
              [:snippet/form
               :snippet/name
               {:snippet/binding [:snippet.binding/symbol
                                  :snippet.binding/form]}]
               snip-id)
        snip (pull (:db/id (:snippet/form bits)))]
    (with-meta snip (merge (meta snip) bits))))

(defn pull-code [sym]
  (let [ns-map (pull-ns (current-branch) (namespace sym))
        snip   (get ns-map (name sym))]
    (pull-snip snip)))

(defn naive-invoke [sym & args]
  (apply (eval (pull-code sym)) args))

;; It remains to stop creating namespaces, and instead create local contexts for
;; each and every function that I need to execute. This is rather hard to
;; swallow. I think the flexibility is worth it though.

;; (defn gen-ns [n]
;;   (find-or-create-ns n)
;;   (binding [*ns* (the-ns n)]
;;     (refer-clojure)
    ;; (require '[ubik.core :refer [create-code-stage lift text-renderer
    ;;                              image-signal source-effector]]
    ;;          '[falloleen.core :as falloleen]
    ;;          '[clojure.pprint :refer [pprint]]))
;;   (the-ns n))

;; (defn gen-ns-for-var [sym]
;;   (let [ns-name (symbol (namespace sym))
;;         ns* (gen-ns ns-name)
;;         ns-vars (map #(symbol (name ns-name) (name (key %)))
;;                      (pull-ns (current-branch) (name ns-name)))]
;;     (binding [*ns* ns*]
;;       (doseq [sym ns-vars]
;;         (declare sym))
;;       (doseq [sym ns-vars]
;;         (clojure.lang.Var/intern ns* (symbol (name sym))
;;                                  (eval (pull-code sym)))))
;;     ns*))

(defn prepare-eval []
  (require '[ubik.core :refer [create-code-stage lift text-renderer
                                 image-signal source-effector]]
             '[falloleen.core :as falloleen]
             '[clojure.pprint :refer [pprint]]))

(defn gen-evalable [sym]
  ;; TODO: Sort ns by transaction time. That should accomplish the same as a
  ;; topological sort.
  (let [ns (pull-ns (current-branch) (namespace sym))]
    `(let [~@(apply concat
              (map (fn [[k v]]
                     [(symbol k) (pull-snip v)])
                   ns))]
       ~(symbol (name sym)))))

;;; Wiping out stale state from the namespaces is, in fact, surprisingly
;;; difficult. You immediately see how tools.namespace came to be so essential
;;; to the entire ecosystem.

;;;;; External API

(def image-signal-in
  (async/chan))

(def image-signal
  (let [ch (async/chan)]
    (async/go-loop []
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
    (intern-code branch sym form)
    (async/put! image-signal-in true)))

(ns ubik.codeless
  (:refer-clojure :exclude [find-ns])
  (:require [clojure.string :as string]
            [datomic.api :as d]
            [ubik.db :refer [push pull conn]]))

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

(defonce ^:private branch
  (atom "master"))

(defn current-branch []
  (str you "/" machine "/" @branch))

(def built-in-code
  (quote
   #:internal.editor.core
   {:edits (fn [ev]
             (:text ev))

    :form {:edit (fn [prev text]
                   (try
                     {:emit (read-string text)}
                     (catch Exception e {:unreadable text})))}

    :display (fn [branch ns n]
               (fn [image]
                 (get-in image [:code branch ns n])))

    :format-code-text
    (fn [form]
      (with-out-str (pprint form)))

    :create-topo-new-editor
    ;; REVIEW: Really verbose, but that might well be the best way. This isn't
    ;; intended for human manipulation.
    ^{:bindings {create-code-stage ubik.core/create-code-stage}}
    (fn [branch ns n]
      (let [{:keys [node event-streams]} (create-code-stage)]

        {:inputs {::image       image-signal
                  ::key-strokes (:key-stroke event-streams)}

         :effectors {::text-render {:in (text-renderer node)}
                     ::code-change {:in (source-effector branch ns n)}}

         :nodes {::code-1 (lift (display branch ns n))
                 ::code-2 (lift format-code-text)
                 ::edits  (lift edits)
                 ::form   form}

         :links #{[::code-1 {:input ::image}]
                  [::code-2 {:input ::code-1}]
                  [::text-render {:in ::code-2}]

                  [::edits {:input ::key-strokes}]
                  [::form {:edit ::edits}]
                  [::code-change {:in ::form}]}}))}))

(defn intern-map [code]
  (push
   (into {}
         (map (fn [[k v]]
                (let [id (push v)
                      ent {:db/id "snip"
                           :snippet/form id
                           :snippet/name (name k)}
                      ent-id (get-in @(d/transact conn [ent])
                                     [:tempids "snip"])]
                  [(keyword (name k))  ent-id])))
         code)))

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

(defn intern-ns [code]
  ;; N.B.: We're assuming that code is a map and the ns of every key is the same
  (let [branch  (current-branch)
        ns-name (namespace (first (keys code)))
        ns-map (intern-map code)]
    (d/transact conn (into  (ns-retraction branch ns-name)
                            [{:db/id "new-ns"
                               :namespace/name ns-name
                              :namespace/map ns-map}
                             {:branch/name branch
                              :branch/namespace "new-ns"}]))))

(defn pull-ns [branch ns-name]
  (pull
   (d/q '[:find ?ns-map .
          :in $ ?branch ?ns-name
          :where
          [?b :branch/name ?branch]
          [?b :branch/namespace ?ns]
          [?ns :namespace/name ?ns-name]
          [?ns :namespace/map ?ns-map]]
        (d/db conn) branch ns-name)))

(defn pull-snip [snip]
  (let [bits (d/pull
              (d/db conn)
              [:snippet/form
               :snippet/name
               {:snippet/binding [:snippet.binding/symbol
                                  :snippet.binding/form]}]
               snip)
        snip (pull (:db/id (:snippet/form bits)))]
    (with-meta snip (merge (meta snip) bits))))

(defn pull-code [sym]
  (let [ns-map (pull-ns (current-branch) (namespace sym))
        snip   (get ns-map (keyword (name sym)))]
    (pull-snip snip)))

(defn invoke [sym & args]
  (apply (eval (pull-code sym)) args))

(defn gen-ns [n]
  (create-ns n)
  (binding [*ns* (the-ns n)]
    (refer-clojure)
    (require '[ubik.core :refer [create-code-stage lift text-renderer
                                 image-signal source-effector]]
             '[falloleen.core :as falloleen]
             '[clojure.pprint :refer [pprint]]))
  (the-ns n))

(defn gen-ns-for-var [sym]
  (let [ns-name (symbol (namespace sym))
        ns* (gen-ns ns-name)
        ns-vars (map #(symbol (name ns-name) (name (key %)))
                     (pull-ns (current-branch) (name ns-name)))]
    (binding [*ns* ns*]
      (doseq [sym ns-vars]
        (declare sym))
      (doseq [sym ns-vars]
        (clojure.lang.Var/intern ns* (symbol (name sym))
                                 (eval (pull-code sym)))))
    ns*))

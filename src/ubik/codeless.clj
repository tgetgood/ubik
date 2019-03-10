(ns ubik.codeless
  (:require [clojure.core.async :as async]
            [ubik.db :as db]
            [ubik.codebase :as dev]
            [ubik.topology :as topo]))

(def built-in-code
  (quote
   [{:edits (fn [ev]
              (:text ev))

     :form {:edit (fn [prev text]
                    (try
                      {:emit (read-string text)}
                      (catch Exception e {:unreadable text})))}

     :display (fn [branch sym]
                (fn [image]
                  (get image (name sym))))

     :format-code-text
     (fn [form]
       (with-out-str (pprint form)))}

    {:create-topo-new-editor
     ;; REVIEW: Really verbose, but that might well be the best way. This isn't
     ;; intended for human manipulation.
     (fn [branch sym]
       (let [{:keys [node event-streams]} (create-code-stage)]

         {:inputs {::image       image-signal
                   ::key-strokes (:key-stroke event-streams)}

          :effectors {::text-render {:in (text-renderer node)}
                      ::code-change {:in (source-effector branch sym)}}

          :nodes {::pull-sym (lift (fn [x] (get x (name sym))))
                  ::code-1   (lift (display branch sym))
                  ::code-2   (lift format-code-text)
                  ::edits    (lift edits)
                  ::form     form}

          :wires #{[::pull-sym {:input ::image}]
                   [::code-1 {:input ::pull-sym}]
                   [::code-2 {:input ::code-1}]
                   [::text-render {:in ::code-2}]

                   [::edits {:input ::key-strokes}]
                   [::form {:edit ::edits}]
                   [::code-change {:in ::form}]}}))}]))

(def initial-topology
  {:inputs    {}
   :effectors {}
   :nodes     {}
   :edges     #{}})

#_(db/reset-db!)

(doseq [code-map built-in-code]
  (doseq [[n form] code-map]
    (dev/intern-code (dev/current-branch) (symbol dev/core-ns (name n)) form)))

(topo/replace-topology! initial-topology)
(async/put! dev/image-signal-in true)

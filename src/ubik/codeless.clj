(ns ubik.codeless
  (:require [clojure.core.async :as async]
            [ubik.db :as db]
            [ubik.codebase :as dev]
            [ubik.topology :as topo]))

(def built-in-code
  (quote
   [{::edits (fn [ev]
              (:text ev))

     ::form {:edit (fn [prev text]
                    (try
                      {:emit (read-string text)}
                      (catch Exception e {:unreadable text})))}

     ::display (fn [branch sym]
                (fn [image]
                  (get image (name sym))))

     ::format-code-text
     (fn [form]
       (with-out-str (pprint form)))}

    ;; Runtimey things
    {:editor.core/image-signal image-signal}]))

(def initial-topology
  {:inputs    {}
   :effectors {}
   :nodes     {}
   :edges     #{}})

(defn snip-edit-topology
  "Creates an editor window and returns a messaging topology to control it."
  [branch sym]
  {:local-code [[::stage        `(create-code-stage ~branch ~sym)]
                [::key-strokes  '(-> stage :event-streams :key-stroke)]
                [::node         '(-> stage :node)]
                [::code-display `(~'display ~branch ~sym)]
                [::text-render  '(text-renderer node)]
                [::code-change  `(~'source-effector ~branch ~sym)]]

   ;; The nodes in a topology are distict process fragments. One function or
   ;; multiplexer map can be instantiated into multiple nodes in the graph, each
   ;; with different internal state and different connections. The same
   ;; computation can mean different things in different contexts.
   :topology {:nodes {::code-1 ::code-display
                      ::code-2 ::format-code-text
                      ::edits  ::edits
                      ::form   ::form}

              ;; Wires connect a set of named inputs to a node. Each name in the
              ;; input map is assumed to also be the name of an input signal to
              ;; the node. If it is not, it will be ignored. Similarly, not all
              ;; signals a node can listen for need to be connected. Whether the
              ;; node can do anything of use without all of its signals is
              ;; application logic.'
              :wires #{[{:in :editor.core/image-signal} ::code-1]
                       [{:in ::code-1} ::code-2]
                       [{:in ::code-2} ::text-render]

                       [{:in ::key-strokes} ::edits]
                       [{:edit ::edits} ::form]
                       [{:in ::form} ::code-change]}}})

;; (db/reset-db!)

(defn register-addition
  "An addition is a new local namespace plus a topology fragment. Unsolved
  problems: cleanup and garbage collection, partial graph updates, linking to
  exisiting graph fragments, access control."
  [{:keys [local-code topology]}]
  (let [branch (dev/current-branch)]
    (doseq [[k form] local-code]
      (dev/intern-code branch (symbol k) form))
    (topo/add-micro-topo topology)))

(defn init! []
  (doseq [code-map built-in-code]
    (doseq [[k form] code-map]
      (dev/intern-code (dev/current-branch) (symbol k) form)))

  (topo/replace-topology! initial-topology)
  (async/put! dev/image-signal-in true))

(ns ubik.codeless
  (:refer-clojure :exclude [intern])
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [datomic.api :as d]
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
  {:local-code [[::stage        `(~'create-code-stage ~branch ~sym)]
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

(defmacro snippet
  "Syntactic sugar for writing linked snippets."
  {:style/indent [1]}
  [bindings expr]
  `{:form '~expr
    :links '~bindings
    :id (java.util.UUID/randomUUID)})

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

(def )

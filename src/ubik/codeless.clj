(ns ubik.codeless
  (:require [ubik.codebase :as code]
            [ubik.codebase.internal :as internal]
            [ubik.process :as process]
            [ubik.topology :as topo]))

(def built-in-code
  (quote
   [{::edits (fn [ev]
              (:text ev))

     ::form {:edit (fn [prev text]
                    (try
                      {:emit (read-string text)}
                      (catch Exception e {:unreadable text})))}

     ::display (fn [sym]
                (fn [image]
                  (get image sym)))

     ::format-code-text
     (fn [form]
       (with-out-str (pprint form)))}

    ;; Runtimey things
    {:editor.core/image-signal image-signal}]))

(def starting-ns
  {:core/display #uuid "81df67e2-01b7-45a7-a22d-a56da9ccd95e"
   :core/format-code-text #uuid "05abc4d9-9c43-4c69-bf43-48d91d714eb5"
   :core/edits #uuid "a179a2af-0243-4f47-96fc-3cc2199ec05e"
   :core/form #uuid "5c3d7b4d-b616-44d8-b6be-f877fd1711df"})

(code/snippet {}
  (fn [image syms]
    (into {} (map (fn [s]
                    (let [ref (get-in image [(namespace s) (name s)])]
                      [(:ns/symbol ref)
                       (invoke-by-id (:id ref))])))
                      syms)))

(def extract-deps
  "I've only named these as vars for the ease of reference"
  (code/snippet {fn-map #uuid "6b0fc0a5-ea99-41b0-b4e2-884f7abe0433"}
    (fn [image]
      (fn-map image [:core/display
                     :core/format-code-text
                     :core/edits
                     :core/form]))))

(def edit-multi
  "Multiplexer that takes inputs from two signals and produces a new signal
  which emits the combination each time either input changes."
  (code/snippet {}
    {:image (fn [{:keys [watch] :as state} image]
              (let [s' (assoc state :image image)]
                (if watch
                  (assoc s' :emit s')
                  s')))
     :watch (fn [{:keys [image] :as state} watch]
             (let [s' (assoc state :watch watch)]
               (if image
                 (assoc s' :emit s')
                 s')))}))

(def snip-edit-topology
  "Creates an editor window and returns a messaging topology to control it."
  (code/snippet {}
    (fn [{{:keys [core/display
                  core/format-code-text
                  core/edits
                  core/form]} :image
          watch               :watch}]
      (let [stage        (create-code-stage watch)
            key-strokes  (-> stage :event-streams :key-stroke)
            text-obj     (-> stage :node)
            code-display (display watch)
            text-render  (text-renderer text-obj)
            code-change  (source-effector watch)]
        {
         ;; The nodes in a topology are distict process fragments. One
         ;; function or multiplexer map can be instantiated into
         ;; multiple nodes in the graph, each with different internal
         ;; state and different connections. The same computation can
         ;; mean different things in different contexts.
         :nodes [(process :ed/code-1 (map code-display))
                 (process :ed/code-2 (map format-code-text))
                 (process :ed/edits (map edits))
                 (make-node :ed/form form)
                 ^{:name :ed/key-strokes} key-strokes

                 (effector :ed/text-render text-render)
                 (effector :ed/code-change code-change)]

         ;; Wires connect a set of named inputs to a node. Each name in
         ;; the input map is assumed to also be the name of an input
         ;; signal to the node. If it is not, it will be
         ;; ignored. Similarly, not all signals a node can listen for
         ;; need to be connected. Whether the node can do anything of
         ;; use without all of its signals is application logic.
         ;; Currently, I'm requiring the wiring diagram to be pure
         ;; data, but I'm allowing the nodes to be compiled things. I
         ;; don't think that's ideal, but I don't know how to resolve
         ;; that yet.  Is the right thing to force all of the local
         ;; bindings to be effectively global, and then refer to the
         ;; snippets that will be converted into runtime constructs by
         ;; id? That seems extreme. But maybe extremism is called
         ;; for...
         :wires #{[:ubik.topology/image :ed/code-1]
                  [:ed/code-1 :ed/code-2]
                  [:ed/code-2 :ed/text-render]

                  [:ed/key-strokes :ed/edits]
                  [{:edit :ed/edits} :ed/form]
                  [:ed/form :ed/code-change]}}))))

(def meta-topo
  (code/snippet {edit-multi   #uuid "5854b093-746e-4d0f-a4c5-84f715354b57"
                 extract-deps #uuid "c6ac1861-bd41-41e4-980e-8fd03e334113"
                 topo-fac     #uuid "e0085372-0ca5-438d-8e51-6a2835cacf1d"}

    {:nodes [(signal :mt/input)
             (process :mt/sub-image (map extract-deps))
             (make-node :mt/combined edit-multi)
             (process :mt/topo (map topo-fac))
             (effector :mt/out topo-effector)]

     :wires #{[:ubik.topology/image :mt/sub-image]
              [{:image :mt/sub-image :watch :mt/input} :mt/combined]
              [:mt/combined :mt/topo]
              [:mt/topo :mt/out]}}))

(defn trigger-network
  "Set off a cascade that should result in something interesting happening. I'm
  becomming less and less discerning in what I consider interesting."
  []
  ;; Refresh all residential code
  (internal/clear-ns)
  (internal/load-ns)

  ;; Setup topology
  (topo/init-topology!
   (internal/invoke-by-id #uuid "9a684f42-a723-4b64-92b2-2a43ad7d643b"))

  ;; Spoof input
  (process/send (:mt/input @topo/node-map) :core/display)
  (process/send (:ubik.topology/image @topo/node-map) (code/internal-ns-map)))

(ns ubik.codeless
  (:require [ubik.codebase :as code]
            [ubik.codebase.internal :as internal]
            [ubik.codebase.storage :as store]
            [ubik.codebase.config :as config]
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


     ::format-code-text
     (fn [form]
       (with-out-str (pprint form)))}

    ]))

(def ns-lookup
  (code/snippet {}
    (fn [image sym]
      (let [ref (get-in image [(namespace sym) (name sym)])]
        [(:ns/symbol ref)
         (edit (:id ref))]))))

(def display
  (code/snippet {lookup {:ref  :core/ns-lookup
                         :time #inst "2019-03-25T18:12:03.485-00:00"}}
    (fn [sym]
      (fn [image]
        (second (lookup image sym))))))


(def starting-ns
  {:core/display (:id display)
   :core/format-code-text #uuid "05abc4d9-9c43-4c69-bf43-48d91d714eb5"
   :core/edits #uuid "a179a2af-0243-4f47-96fc-3cc2199ec05e"
   :core/form #uuid "5c3d7b4d-b616-44d8-b6be-f877fd1711df"
   :core/ns-lookup (:id ns-lookup)})

(code/populate-nses starting-ns)

(def fn-map
  (code/snippet {ns-lookup #uuid "3f1615d8-3146-4eb2-94cd-232ff57e5dcc"}
    (fn [image syms]
      (into {} (map (partial ns-lookup image)) syms))))

(def extract-deps
  "I've only named these as vars for the ease of reference"
  (code/snippet {fn-map #uuid "6dc2b544-50d1-4326-8d6e-fd736a21dd7a"}
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
                 key-strokes
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

                  [[:ubik.events/text-area watch :key-stroke] :ed/edits]
                  [{:edit :ed/edits} :ed/form]
                  [:ed/form :ed/code-change]}}))))

(def meta-topo
  (code/snippet {edit-multi   #uuid "5854b093-746e-4d0f-a4c5-84f715354b57"
                 extract-deps #uuid "162444b2-dbf7-49c2-bd3d-d778793d2f12",
                 topo-fac     #uuid "0fa69760-841f-4ff8-ad41-c97d9788dbd0"}

    {:nodes [(signal :mt/input)
             (process :mt/sub-image (map extract-deps))
             (make-node :mt/combined edit-multi)
             (process :mt/topo (map topo-fac))
             (effector :mt/out topo-effector)]

     :wires #{[:ubik.topology/image :mt/sub-image]
              [{:image :mt/sub-image :watch :mt/input} :mt/combined]
              [:mt/combined :mt/topo]
              [:mt/topo :mt/out]}}))

(def dependencies
  [meta-topo ])

(defn trigger-network
  "Set off a cascade that should result in something interesting happening. I'm
  becomming less and less discerning in what I consider interesting."
  []
  ;; Refresh all residential code
  (internal/clear-ns)
  (internal/load-ns)

  ;; Setup topology
  (topo/init-topology!
   (internal/invoke-by-id #uuid "45c7c2eb-a823-4c58-8cec-4734f0e104a7"))
  ;; Spoof input
  (process/send (:mt/input @topo/node-map) :core/display)
  (process/send (:ubik.topology/image @topo/node-map) (code/internal-ns-map)))

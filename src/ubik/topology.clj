(ns ubik.topology
  (:require [clojure.core.async :as async]
            [clojure.set :as set]
            [clojure.string :as string]
            [datomic.api :as d]
            [ubik.codebase :as cb :refer [current-branch]]
            [ubik.db :refer [push pull conn]]))

(defn set-topology! [tops]
  (let [topo-id (push tops)]
    (d/transact conn
                [{:branch/name     (current-branch)
                  :branch/topology topo-id}])))

(defn current-topology []
  (pull (d/q '[:find ?t .
               :in $ ?b
               :where
               [?bid :branch/name ?b]
               [?bid :branch/topology ?t]]
             (d/db conn)
             (current-branch))))

(defn vmap [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn listen [x]
  {:out (async/mult x)})

(defn lift [f]
  {:in (fn [_ x]
         (let [out (f x)]
           (when out
             {:emit out})))})

(defn form->multiplex [form]
  (let [o (eval form)]
    (cond
      (fn? o) (lift o)
      (map? o) o
      :else (throw (Exception. "Bad multiplexer")))))

(defn process [k]
  (let [form (cb/gen-evalable (symbol k))
        mul (form->multiplex form)
        out-ch (async/chan (async/sliding-buffer 128))
        in-map (vmap (fn [_] (async/chan)) mul)]
    (doseq [[k ch] in-map]
      (async/go-loop [state {}]
        (when-let [msg (async/<! ch)]
          (let [res (atom state)]
            (try
              (reset! res ((get mul k) state msg)
                      (when (contains? res :emit)
                        (async/>! out-ch (:emit res)))
                      (when (contains? res :emit-all)
                        (run! #(async/put! out-ch %) (:emit-all res))))
              (catch Exception e (reset! res state)))
            (recur @res)))))
    {:in in-map :out (async/mult out-ch)}))

(defn wire [channels send-map receiver]
  (doseq [[edge sender] send-map]
    (let [send-ch (get-in channels [sender :out])
          rec-ch (get-in channels [receiver :in edge])
          tch (async/chan)]
      (async/tap send-ch tch)
      (async/pipe tch rec-ch))))

(defn activate!
  "As a first step, we're just going to start the new topology and let the old
  ones lay around.

  Inputs can be listened to, returning channels, effectors are assumed to be
  receiving channels, and nodes, when processed, become multiplexers which can
  be wired together.

  O! this is so ad hoc."
  [{:keys [nodes wires]}]
  (let [node-chans  (vmap process nodes)]
    (doseq [[send-map receiver] wires]
      (wire node-chans send-map receiver))))

(defn replace-topology! [t]
  (set-topology! t)
  (activate! t))

(defn add-micro-topo [{:keys [nodes wires]}]
  (let [current (current-topology)
        new {:nodes (merge (:nodes current) nodes)
             :wires (set/union (:wires current) wires)}]
    (replace-topology! new)))

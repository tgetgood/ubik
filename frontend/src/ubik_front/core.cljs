(ns ubik-front.core
  (:require [cljs.core.async :as async]
            [falloleen.core :as f]
            [falloleen.hosts :as hosts]
            [paren-soup.core :as soup]
            [ubik-front.events :as events]
            [ubik-front.ws :as ws]))

(def ws
  "The websocket connection to the server."
  (atom nil))

(defonce last-text (atom ""))

(defn edit-cb [ev]
  (when (= "keyup" (.-type ev))
    (let [text (.-textContent (.-currentTarget ev))]
      (when-not (= text @last-text)
        (reset! last-text text)
        (when @ws
          (async/put! @ws {:signal :ubik.server/edits
                           :message text}))))))

(def editor
  (soup/init (js/document.getElementById "paren-soup")
             (clj->js {:before-change-callback nil
                       :disable-undo-redo? false
                       :change-callback edit-cb
                       :console-callback nil
                       :compiler-fn nil})))

(def host
  (hosts/default-host {:size :fullscreen}))

(def last-draw (atom nil))

(defn render! [im]
  (when-not (= im @last-draw)
    (reset! last-draw im)
    (f/draw! im host)))

(defn set-editor! [text]
  (set! (.-innerText (.-content editor)) text)
  (soup/initialize! editor))

(defn process [{:keys [signal message] :as pack}]
  (cond
    (= signal :ubik.topology/screen) (render! message)
    (= signal :ubik.topology/code) (set-editor! message)))

(defn init! []
  (let [b (ws/ws-ch "ws://localhost:8080")]
    (async/go
      (let [ch (:ws-channel (async/<! b))]
        (reset! ws ch)
        (events/monitor host ch)
        (async/reduce (fn [_ x] (process x)) nil ch)))))

(init!)

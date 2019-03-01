(ns ubik.events
  "Respond to javafx events. This shouldn't be in this project."
  (:require [clojure.core.async :as async]
            [editor.jfx :as fx])
  (:import [javafx.event Event EventHandler]
           javafx.scene.control.TextArea
           javafx.scene.input.KeyEvent))

(defmacro handler
  {:style/indent [1]}
  [binding & body]
  `(proxy [Object EventHandler] []
     (handle [^Event ~@binding]
       ~@body)))

(defmulti datafy-event (fn [x ^Event ev] (.getName (.getEventType ev))))

(defmethod datafy-event :default
  [x ev]
  ev)

(defmethod datafy-event "MOUSE_ENTERED"
  [x ev]
  ev)

(defmethod datafy-event "MOUSE_EXITED"
  [x ev]
  ev)

(defmethod datafy-event "KEY_TYPED"
  [^TextArea x ^KeyEvent ev]
  {:caret (.getCaretPosition x)
   :char (.getCharacter ev)
   :typed (.getText ev)
   :text (.getText x)})

(def binding-map
  {:mouse-down 'setOnMousePressed
   :mouse-up   'setOnMouseReleased
   :mouse-in   'setOnMouseEntered
   :mouse-out  'setOnMouseExited
   :click      'setOnMousePressed

   :key-down   'setOnKeyPressed
   :key-up     'setOnKeyReleased
   :key-stroke 'setOnKeyTyped})

(defn ch-handler [event-xform]
  (let [c (async/chan (async/sliding-buffer 128) (map event-xform))]
    [(handler [ev] (async/put! c ev)) c]))

(defmacro binder
  "Binds event handlers to the given JFX object and returns a map from event
  names to channels that will receive events."
  [tag ev-parse]
  (let [x (with-meta (gensym) {:tag tag})]
    `(fn [~x]
       (into {}
             [~@(map
                 (fn [[k# v#]]
                   (let [hs (gensym)
                         cs (gensym)]
                     `(let [[~hs ~cs] (ch-handler (partial ~ev-parse ~x))]
                        (fx/fx-thread (. ~x ~v# ~hs))
                        [~k# ~cs])))
                 binding-map)]))))

(defmacro bind-text-area!
  [node]
  `((binder TextArea datafy-event) ~node))

;; (defmacro bind-canvas!
;;   "This is more than a little ugly."
;;   {:style/indent [1]}
;;   [node handlers]
;;   `((create-binder Scene ~handlers) ~node))

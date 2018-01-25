(ns lemonade.hosts.protocol)

(defprotocol Host
  (event-system [this])
  (render-fn [this])
  (width [this] "Current frame width")
  (height [this] "Current frame height")
  (on-resize [this cb] "Invoke cb when host is resized")
  (resize-frame [this [width height]])
  (fullscreen [this]))

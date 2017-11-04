(defproject lemonade "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/spec.alpha "0.1.134"]
                 [quil "2.6.0"]]

  :plugins [[lein-figwheel "0.5.13"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :cljsbuild {:builds
              [{:id "canvas"
                :source-paths ["src"]

                :figwheel {:on-jsload "lemonade.demos.canvas/on-js-reload"}

                :compiler {:main lemonade.demos.canvas
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/canvas.js"
                           :output-dir "resources/public/js/compiled/out"
                           :parallel-build true
                           :source-map-timestamp true
                           :preloads [devtools.preload]}}
               {:id           "min"
                :source-paths ["src"]
                :compiler     {:main            lemonade.demos.canvas
                               :output-to       "resources/public/js/compiled/app.js"
                               :optimizations   :advanced
                               :parallel-build  true
                               :closure-defines {goog.DEBUG false}
                               :pretty-print    false}}]}

  :figwheel {:css-dirs ["resources/public/css"]}


  :profiles {:dev {:dependencies [[binaryage/devtools "0.9.4"]
                                  [org.clojure/tools.namespace "0.2.11"]
                                  [figwheel-sidecar "0.5.13"]
                                  [com.cemerick/piggieback "0.2.2"]
                                  [org.clojure/test.check "0.9.0"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]

                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   ;; need to add the compliled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     :target-path]}})

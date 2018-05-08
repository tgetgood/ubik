(defproject macroexpanse/ubik "0.5.1-SNAPSHOT"
  :description "High level language for graphical and UI programming. No markup."
  :url "https://github.com/tgetgood/ubik"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :scm {:name "git"
        :url  "https://github.com/tgetgood/ubik"}

  :deploy-repositories [["releases" :clojars]]

  :min-lein-version "2.7.1"

  :dependencies [[net.cgrand/macrovich "0.2.1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.async "0.3.465"]
                 [quil "2.6.0" :exclusions [[org.clojure/clojure]]]]

  :plugins [[lein-figwheel "0.5.14"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src"]

  :cljsbuild
  {:builds
   [{:id           "canvas"
     :source-paths ["src"]
     :compiler     {:main                 ubik.core
                    :asset-path           "js/compiled/out"
                    :output-to            "resources/public/js/compiled/ubik.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :parallel-build       true
                    :source-map-timestamp true
                    :checked-arrays       :warn
                    :preloads             [devtools.preload]}}]}

  :profiles
  {:dev {:dependencies  [[binaryage/devtools "0.9.9"]
                         [com.cemerick/piggieback "0.2.2"]
                         [figwheel-sidecar "0.5.14"
                          :exclusions [org.clojure/core.async]]
                         [org.clojure/test.check "0.9.0"]
                         [org.clojure/tools.namespace "0.2.11"]]
         ;; need to add dev source path here to get user.clj loaded
         :source-paths  ["src" "dev"]

         :repl-options  {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
         ;; need to add the compliled assets to the :clean-targets
         :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                           :target-path]}})

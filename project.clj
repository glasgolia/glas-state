(defproject glas-state "0.1.0-alpha"
  :description "Statecharts for clojure and clojurescript, with an xstate-like api."
  :url "https://github.com/glasgolia/glas-state"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"]
                 [org.clojure/core.async "0.4.500"]]
  :repl-options {:init-ns glasgolia.glas-state.core}
  :plugins [[lein-cljsbuild "1.1.7"]]

  :profiles
  {:dev
            {:dependencies []

             :plugins      []}


   :prod { }
   }


  :cljsbuild {:builds {:app
                       {:source-paths ["src" "env/dev/cljs"]
                        :compiler
                                      {:main          "glasgolia.glas-state.core"
                                       :output-to     "resources/public/js/app.js"
                                       :output-dir    "resources/public/js/out"
                                       :asset-path    "js/out"
                                       :source-map    true
                                       :optimizations :none
                                       :pretty-print  true}

                                      }
                       :release
                       {:source-paths ["src" "env/prod/cljs"]
                        :compiler
                                      {:output-to     "resources/public/js/app.js"
                                       :output-dir    "resources/public/js/release"
                                       :optimizations :advanced
                                       :infer-externs true
                                       :pretty-print  false}}}})




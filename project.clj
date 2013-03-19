(defproject tictactoe "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :cljsbuild {
    :builds [{
      :source-paths ["src/tictactoe"]
      :compiler {
        :output-to "resources/static/ttt-canvas.js"
        :optimizations :advanced
        :pretty-print false}}]})

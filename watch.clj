(require 'cljs.build.api)

(cljs.build.api/watch
 "src"
 {:main 'tac.core
  :output-to "out/main.js"})
(require 'cljs.build.api)

(cljs.build.api/build
 "src"
 {:main 'tac.core
  :output-to "out/main.js"})
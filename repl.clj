(require 'cljs.repl)
(require 'cljs.build.api)
(require 'cljs.repl.browser)
(require 'clojure.set)

(cljs.build.api/build
 "src"
 {:main 'tac.core
  :output-to "out/main.js"
  :verbose true})

(cljs.repl/repl
 (cljs.repl.browser/repl-env)
 :watch "src"
 :output-dir "out")
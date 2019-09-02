(require '[clojure.java.shell :as shell]
         '[clojure.main])
(require '[rebel-readline.core]
         '[rebel-readline.clojure.main]
         '[rebel-readline.clojure.line-reader]
         '[rebel-readline.clojure.service.local]
         '[rebel-readline.cljs.service.local]
         '[rebel-readline.cljs.repl])
(require '[cljs.build.api :as api]
         '[cljs.repl :as repl]
         '[cljs.repl.node :as node])

(defmulti task first)

(defmethod task :default
  [args]
  (let [all-tasks  (-> task methods (dissoc :default) keys sort)
        interposed (->> all-tasks (interpose ", ") (apply str))]
    (println "Unknown or missing task. Choose one of:" interposed)
    (System/exit 1)))

(defmethod task "repl:jvm"
  [args]
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
     (rebel-readline.clojure.service.local/create))
    (clojure.main/repl
     :prompt (fn []) ;; prompt is handled by line-reader
     :read (rebel-readline.clojure.main/create-repl-read))))

(defmethod task "repl:node"
  [args]
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
     (rebel-readline.cljs.service.local/create))
    (cljs.repl/repl
     (node/repl-env)
     :prompt (fn []) ;; prompt is handled by line-reader
     :read (rebel-readline.cljs.repl/create-repl-read)
     :output-dir "out"
     :cache-analysis false)))

(def build-options
  {:main 'struct.tests-main
   :output-to "out/tests.js"
   :output-dir "out/tests"
   :target :nodejs
   :pretty-print true
   :optimizations :advanced
   :language-in  :ecmascript5
   :language-out :ecmascript5
   :pseudo-names true
   :verbose true})

(defmethod task "build:tests"
  [args]
  (api/build (api/inputs "src" "test") build-options))


(defmethod task "build:benchmarks"
  [args]
  (api/build (api/inputs "src" "test")
             (assoc build-options
                    :main 'struct.benchmarks
                    :output-to "out/bench.js"
                    :output-dir "out/bench")))


;;; Build script entrypoint. This should be the last expression.

(task *command-line-args*)

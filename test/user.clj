(ns user
  (:require [clojure.tools.namespace.repl :as r]
            [clojure.test :as test]))

(defn run-tests
  ([] (run-tests #"^struct.tests.*"))
  ([o]
   (r/refresh)
   (cond
     (instance? java.util.regex.Pattern o)
     (test/run-all-tests o)

     (symbol? o)
     (if-let [sns (namespace o)]
       (do (require (symbol sns))
           (test/test-vars [(resolve o)]))
       (test/test-ns o)))))

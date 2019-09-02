(ns struct.tests-main
  (:require
   #?(:cljs [cljs.test :as t]
      :clj [clojure.test :as t])
   [struct.tests-core]
   [struct.tests-alpha]))

#?(:cljs
   (do
     (enable-console-print!)
     (set! *main-cli-fn* #(t/run-tests 'struct.tests-core
                                       'struct.tests-alpha)))

   :clj
   (defn -main
     [& args]
     (let [{:keys [fail]} (t/run-all-tests #"^struct.tests.*")]
       (if (pos? fail)
         (System/exit fail)
         (System/exit 0)))))

#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests]
     [m]
     (if (t/successful? m)
       (set! (.-exitCode js/process) 0)
       (set! (.-exitCode js/process) 1))))

(ns struct.tests
  (:require #?(:cljs [cljs.test :as t]
               :clj [clojure.test :as t])
            [struct.core :as st]))

;; --- Tests

(t/deftest test-optional-validators
  (let [scheme {:max st/number
                :scope st/string}
        input {:scope "foobar"}
        result (st/validate input scheme)]
    (t/is (= nil (first result)))
    (t/is (= input (second result)))))

(t/deftest test-simple-validators
  (let [scheme {:max st/number
                :scope st/string}
        input {:scope "foobar" :max "d"}
        errors {:max '("must be a number")}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-simple-validators-with-vector-schema
  (let [scheme [[:max st/number]
                [:scope st/string]]
        input {:scope "foobar" :max "d"}
        errors {:max '("must be a number")}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-multiple-validators
  (let [scheme {:max [st/required st/number]
                :scope st/string}
        input {:scope "foobar"}
        errors {:max '("this field is mandatory")}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-validation-with-coersion
  (let [scheme {:max st/number-str
                :scope st/string}
        input {:max "2" :scope "foobar"}
        result (st/validate input scheme)]
    (t/is (= nil (first result)))
    (t/is (= {:max 2 :scope "foobar"} (second result)))))

(t/deftest test-validation-with-custom-coersion
  (let [scheme {:max [[st/number-str :coerce (constantly :foo)]]
                :scope st/string}
        input {:max "2" :scope "foobar"}
        result (st/validate input scheme)]
    (t/is (= nil (first result)))
    (t/is (= {:max :foo :scope "foobar"} (second result)))))

;; --- Entry point

#?(:cljs (enable-console-print!))
#?(:cljs (set! *main-cli-fn* #(t/run-tests)))
#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests]
     [m]
     (if (t/successful? m)
       (set! (.-exitCode js/process) 0)
       (set! (.-exitCode js/process) 1))))

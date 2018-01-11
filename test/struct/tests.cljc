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
        errors {:max "must be a number"}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-neested-validators
  (let [scheme {[:a :b] st/number
                [:c :d :e] st/string}
        input {:a {:b "foo"} :c {:d {:e "bar"}}}
        errors {:a {:b "must be a number"}}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:c {:d {:e "bar"}}} (second result)))))

(t/deftest test-single-validators
  (let [result1 (st/validate-single 2 st/number)
        result2 (st/validate-single nil st/number)
        result3 (st/validate-single nil [st/required st/number])]
    (t/is (= [nil 2] result1))
    (t/is (= [nil nil] result2))
    (t/is (= ["this field is mandatory" nil] result3))))

(t/deftest test-parametric-validators
  (let [result1 (st/validate
                  {:name "foo"}
                  {:name [[st/min-count 4]]})
        result2 (st/validate
                  {:name "bar"}
                  {:name [[st/max-count 2]]})]
    (t/is (= {:name "less than the minimum"} (first result1)))
    (t/is (= {:name "longer than the maximum"} (first result2)))))

(t/deftest test-simple-validators-with-vector-schema
  (let [scheme [[:max st/number]
                [:scope st/string]]
        input {:scope "foobar" :max "d"}
        errors {:max "must be a number"}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-simple-validators-with-translate
  (let [scheme [[:max st/number]
                [:scope st/string]]
        input {:scope "foobar" :max "d"}
        errors {:max "a"}
        result (st/validate input scheme {:translate (constantly "a")})]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-dependent-validators-1
  (let [scheme [[:password1 st/string]
                [:password2 [st/identical-to :password1]]]
        input {:password1 "foobar"
               :password2 "foobar."}
        errors {:password2 "does not match"}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:password1 "foobar"} (second result)))))

(t/deftest test-dependent-validators-2
  (let [scheme [[:password1 st/string]
                [:password2 [st/identical-to :password1]]]
        input {:password1 "foobar"
               :password2 "foobar"}
        result (st/validate input scheme)]
    (t/is (= nil (first result)))
    (t/is (= {:password1 "foobar"
              :password2 "foobar"} (second result)))))

(t/deftest test-multiple-validators
  (let [scheme {:max [st/required st/number]
                :scope st/string}
        input {:scope "foobar"}
        errors {:max "this field is mandatory"}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-validation-with-coersion
  (let [scheme {:max st/integer-str
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

(t/deftest test-validation-with-custom-message
  (let [scheme {:max [[st/number-str :message "custom msg"]]
                :scope st/string}
        input {:max "g" :scope "foobar"}
        errors {:max "custom msg"}
        result (st/validate input scheme)]
    (t/is (= errors (first result)))
    (t/is (= {:scope "foobar"} (second result)))))

(t/deftest test-coersion-with-valid-values
  (let [scheme {:a st/number-str
                :b st/integer-str}
        input {:a 2.3 :b 3.3}
        [errors data] (st/validate input scheme)]
    (t/is (= {:a 2.3 :b 3} data))))

(t/deftest test-validation-neested-data-in-a-vector
  (let [scheme {:a [st/vector [st/every number?]]}
        input1 {:a [1 2 3 4]}
        input2 {:a [1 2 3 4 "a"]}
        [errors1 data1] (st/validate input1 scheme)
        [errors2 data2] (st/validate input2 scheme)]
    (t/is (= data1 input1))
    (t/is (= errors1 nil))
    (t/is (= data2 {}))
    (t/is (= errors2 {:a "must match the predicate"}))))

;; --- Entry point

#?(:cljs
   (do
     (enable-console-print!)
     (set! *main-cli-fn* #(t/run-tests))))

#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests]
     [m]
     (if (t/successful? m)
       (set! (.-exitCode js/process) 0)
       (set! (.-exitCode js/process) 1))))

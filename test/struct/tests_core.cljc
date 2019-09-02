(ns struct.tests-core
  (:require
   ;; #?(:clj  [criterium.core :refer [quick-bench]])
   ;; #?(:cljs [cljs.spec.alpha :as s]
   ;;    :clj  [clojure.spec.alpha :as s])
   #?(:cljs [cljs.test :as t]
      :clj [clojure.test :as t])
   [struct.core :as st]))

;; --- Tests

(t/deftest test-optional-validators
  (let [scheme {:max st/number
                :scope st/string}
        input {:scope "foobar"}
        result (st/validate scheme input)]
    (t/is (= nil (first result)))
    (t/is (= input (second result)))))

(t/deftest test-simple-validators
  (let [scheme {:max st/number
                :scope st/string}
        input {:scope "foobar" :max "d"}
        [error data] (st/validate scheme input)]
    (t/is (map? error))
    (t/is (= (get-in error [:max :code]) ::st/number))
    (t/is (map? data))
    (t/is (= (get data :scope) "foobar"))))

(t/deftest test-predicate-validators
  (let [scheme {:max [st/required number?]}
        input1 {:max "foo"}
        input2 {:max 2}
        [error1 data1] (st/validate scheme input1)
        [error2 data2] (st/validate scheme input2)]

    (t/is (map? error1))
    (t/is (map? data1))
    (t/is (empty? data1))
    (t/is (= (get-in error1 [:max :code]) ::st/custom-predicate))

    (t/is (nil? error2))
    (t/is (map? data2))
    (t/is (= (get data2 :max) 2))))

(t/deftest test-neested-validators
  (let [scheme {[:a :b] st/number
                [:c :d :e] st/string}
        input {:a {:b "foo"} :c {:d {:e "bar"}}}
        [errors data] (st/validate scheme input)]
    (t/is (map? errors))
    (t/is (= (get-in errors [:a :b :code]) ::st/number))
    (t/is (map? data))
    (t/is (= (get-in data [:c :d :e]) "bar"))))

(t/deftest test-parametric-validators
  (let [[errors1 data1] (st/validate {:name [[st/min-count 4]]} {:name "foo"})
        [errors2 data2] (st/validate {:name [[st/max-count 2]]} {:name "bar"})]
    (t/is (map? data1))
    (t/is (map? data2))
    (t/is (empty? data1))
    (t/is (empty? data2))
    (t/is (= (get-in errors1 [:name :code]) ::st/min-count))
    (t/is (= (get-in errors2 [:name :code]) ::st/max-count))
    (t/is (= (get-in errors1 [:name :value]) "foo"))
    (t/is (= (get-in errors2 [:name :value]) "bar"))))

(t/deftest test-simple-validators-with-vector-schema
  (let [scheme [[:max st/number]
                [:scope st/string]]
        input {:scope "foobar" :max "d"}
        [errors data] (st/validate scheme input)]

    (t/is (map? errors))
    (t/is (= (get-in errors [:max :code]) ::st/number))
    (t/is (map? data))
    (t/is (= (get data :scope) "foobar"))))

(t/deftest test-simple-validators-message
  (let [scheme [[:max [st/number :message (constantly "a")]]
                [:scope [st/string :message (constantly "b")]]]
        input {:scope "foobar" :max "d"}
        [errros data] (st/validate scheme input)]
    (t/is (map? errros))
    (t/is (map? data))
    (t/is (= (get-in errros [:max :code]) ::st/number))
    (t/is (= (get-in errros [:max :message]) "a"))
    (t/is (= (get data :scope) "foobar"))))

(t/deftest test-dependent-validators-1
  (let [scheme [[:password1 st/string]
                [:password2 [st/identical-to :password1]]]
        input {:password1 "foobar"
               :password2 "foobar."}
        [errors data] (st/validate scheme input)]
    (t/is (map? errors))
    (t/is (map? data))
    (t/is (= (get-in errors [:password2 :code]) ::st/identical-to))
    (t/is (= (get data :password1) "foobar"))))

(t/deftest test-dependent-validators-2
  (let [scheme [[:password1 st/string]
                [:password2 [st/identical-to :password1]]]
        input {:password1 "foobar"
               :password2 "foobar"}
        [errors data] (st/validate scheme input)]
    (t/is (nil? errors))
    (t/is (map? data))
    (t/is (= (get data :password1) "foobar"))
    (t/is (= (get data :password2) "foobar"))))

(t/deftest test-multiple-validators
  (let [scheme {:max [st/required st/number]
                :scope st/string}
        input {:scope "foobar"}
        [errors data] (st/validate scheme input)]
    (t/is (map? errors))
    (t/is (map? data))
    (t/is (= (get-in errors [:max :code]) ::st/required))
    (t/is (= (get data :scope) "foobar"))))

(t/deftest test-validation-with-coersion
  (let [scheme {:max [st/required st/integer-str]
                :scope [[st/string :coerce (constantly :foo)]]}
        input {:max "2" :scope "foobar"}
        [errors data] (st/validate scheme input)]

    (t/is (nil? errors))
    (t/is (map? data))
    (t/is (= (get data :max) 2))
    (t/is (= (get data :scope) :foo))))

(t/deftest test-validation-nested-data-in-a-vector
  (let [scheme {:a [st/vector [st/every number?]]}
        input1 {:a [1 2 3 4]}
        input2 {:a [1 2 3 4 "a"]}
        [errors1 data1] (st/validate scheme input1)
        [errors2 data2] (st/validate scheme input2)]


    (t/is (map? data1))
    (t/is (nil? errors1))
    (t/is (= (get data1 :a) [1 2 3 4]))

    (t/is (map? data2))
    (t/is (map? errors2))
    (t/is (empty? data2))
    (t/is (= (get-in errors2 [:a :code] ::st/every)))))

;; --- Entry point

#?(:cljs
   (do
     (enable-console-print!)
     (set! *main-cli-fn* #(t/run-tests)))
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

(ns struct.tests-alpha
  (:require
   #?(:cljs [cljs.test :as t]
      :clj [clojure.test :as t])
   [struct.alpha :as st]))

;; --- Tests

(st/defs ::number number?)
(st/defs ::string string?)

(t/deftest test-simple-specs
  ;; Basic conform
  (t/is (= 1 (st/conform ::number 1)))
  (t/is (= "a" (st/conform ::string "a")))
  (t/is (= "" (st/conform ::string "")))

  ;; Invalid conform
  (t/is (= ::st/invalid (st/conform ::number "1")))
  (t/is (= ::st/invalid (st/conform ::string nil)))

  (t/is (= [{:path [], :name ::number, :val "1" :via [::number]}]
           (st/explain ::number "1")))
  (t/is (= [{:path [], :name ::string, :val nil :via [::string]}]
           (st/explain ::string nil))))

(st/defs ::number-opt (st/opt ::number))
(st/defs ::string-opt (st/opt ::string))

(t/deftest test-conform-with-optionals
  (t/is (= 1 (st/conform ::number-opt 1)))
  (t/is (= nil (st/conform ::number-opt nil)))
  (t/is (= "a" (st/conform ::string-opt "a")))
  (t/is (= "" (st/conform ::string-opt "")))
  (t/is (= nil (st/conform ::string-opt nil))))

(st/defs ::positive pos?)
(st/defs ::positive-number (st/&& ::number ::positive))

(t/deftest test-conform-with-and
  (t/is (= 1 (st/conform ::positive-number 1)))
  (t/is (= ::st/invalid (st/conform ::positive-number -1)))
  (t/is (= ::st/invalid (st/conform ::positive-number "1")))

  (t/is (= [{:path [], :name ::number, :val nil :via [::positive-number ::number]}]
           (st/explain ::positive-number nil)))
  (t/is (= [{:path [], :name ::number, :val "1" :via [::positive-number ::number]}]
           (st/explain ::positive-number "1")))
  (t/is (= [{:path [], :name ::positive, :val -1 :via [::positive-number ::positive]}]
           (st/explain ::positive-number -1)))
)

(st/defs ::author
  (st/dict :name ::string))

(st/defs ::book
  (st/dict :name ::string
           :age ::number
           :author ::author))

(t/deftest test-complex-dict
  (let [data {:author {:name "Andrej Sapkowski"}
              :name "Baptism of Fire"
              :age 1996}
        wdata {:name "Baptism of Fire"
               :author {}
               :age "1996"}]
    (t/is (= data
             (st/conform ::book data)))
    (t/is (= ::st/invalid
             (st/conform ::book wdata)))

    (t/is (= [{:path [:age],
               :name ::number,
               :val "1996",
               :via [::book ::number]}
              {:path [:author :name],
               :name ::string,
               :val nil,
               :via [::book ::author ::string]}]
             (st/explain ::book wdata)))))

(st/defs ::authors (st/coll-of ::author))

(t/deftest test-coll-of
  (let [data [{:name "Andrej Sapkowski"}]
        wdata [{:name :foobar}]]
    (t/is (= data
             (st/conform ::authors data)))

    (t/is (= ::st/invalid
             (st/conform ::authors wdata)))

    (t/is (= [{:path [0],
               :name ::author,
               :cause {:path [:name],
                       :name ::string,
                       :val :foobar,
                       :via [::authors ::author ::string]},
               :via [::authors ::author],
               :val {:name :foobar}}]
             (st/explain ::authors wdata)))))





(ns struct.benchmarks
  (:require
   #?(:clj  [criterium.core :refer [quick-bench]])
   #?(:cljs [cljs.spec.alpha :as s]
      :clj  [clojure.spec.alpha :as s])
   #?(:cljs [cljs.test :as t])
   [struct.alpha :as st]))

(def email-rx #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$")

(defn email?
  [v]
  (and (string? v)
       (re-matches email-rx v)))

(s/def ::username string?)
(s/def ::age number?)
(s/def ::email email?)

(s/def ::bench-form
  (s/keys :req-un [::username ::age ::email]))

(st/defs ::bench-form
  (st/dict :username ::st/string
           :age ::st/number
           :email ::st/email))

(defn bench-valid-using-spec
  []
  (let [data {:username "foo" :age 10 :email "foo@bar.com"}
        bench-fn (fn [data]
                   (let [result (s/valid? ::bench-form data)]
                     (assert result "should be valid")))]

    #?(:cljs
       (simple-benchmark [data data]
         (bench-fn data)
         100000)

       :clj
       (quick-bench (bench-fn data)))))

(defn bench-valid-using-struct
  []
  (let [data {:username "foo" :age 10 :email "foo@bar.com"}
        bench-fn (fn [data]
                   (let [result (st/valid? ::bench-form data)]
                     (assert result "should be valid")))]

    #?(:cljs
       (simple-benchmark [data data]
         (bench-fn data)
         100000)

       :clj
       (quick-bench (bench-fn data)))))

#?(:cljs
   (do
     (enable-console-print!)
     (set! *main-cli-fn*
           #(do
              (println "Benchmark (valid?) - clojure.spec")
              (bench-valid-using-spec)
              (println "Benchmark (valid?) - struct.alpha")
              (bench-valid-using-struct)))))



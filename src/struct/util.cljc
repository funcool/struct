(ns struct.util
  (:require #?(:cljs [cljs.reader :as edn]
               :clj  [clojure.edn :as edn])))

(defn numeric?
  [s]
  (when (string? s)
    (boolean (re-matches #"^[+-]?([0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)([eE][+-]?[0-9]+)?$" s))))

(defn parse-number
  [s]
  (when (numeric? s)
    (edn/read-string s)))

(defn parse-int
  "Return the number value in integer form."
  [s]
  (cond
    (number? s)
    (int s)

    (numeric? s)
    #?(:clj (.longValue (java.math.BigDecimal. ^String s))
       :cljs (js/parseInt s 10))

    :else nil))


(ns struct.alpha
  ;; (:refer-clojure :exclude [and])
  (:require [struct.util :as util])
  #?(:cljs (:require-macros [struct.alpha :refer [defs]])))

(def ^:dynamic *registry* (atom {}))

;; --- Impl

(defprotocol ISpec
  (-conform [it val])
  (-explain [it path val]))

(defrecord FnSpec [pred name coerce]
  ISpec
  (-conform [_ value]
    (if (pred value)
      (if (fn? coerce)
        (coerce value)
        value)
      ::invalid))
  (-explain [self path val]
    (if (= ::invalid (-conform self val))
      [{:path path :name name :val val}]
      [])))

(defrecord AndSpec [specs name]
  ISpec
  (-conform [_ value]
    (reduce (fn [acc s]
              (let [result (-conform s acc)]
                (if (= result ::invalid)
                  (reduced ::invalid)
                  result)))
            value
            specs))

  (-explain [_ path val]
    (let [[val errors] (reduce (fn [[val _] s]
                                 (let [res (-conform s val)]
                                   (if (= res ::invalid)
                                     (reduced [nil (-explain s path val)])
                                     [res nil])))
                               [val nil]
                               specs)]
      errors)))

(defrecord OptSpec [spec name]
  ISpec
  (-conform [_ data]
    (if (nil? data)
      data
      (-conform spec data)))

  (-explain [_ path data]
    (if (nil? data)
      []
      (-explain spec path data))))

(defrecord MapSpec [pairs name]
  ISpec
  (-conform [_ data]
    (if-not (map? data)
      ::invalid
      (reduce (fn [acc [k s]]
                (let [val (get data k)
                      res (-conform s val)]
                  (if (= res ::invalid)
                    (reduced ::invalid)
                    (assoc acc k res))))
              {}
              pairs)))

  (-explain [_ path data]
    (if (map? data)
      (reduce (fn [acc [k s]]
                (into acc (-explain s (conj path k) (get data k))))
              []
              pairs)
      (if (empty? path)
        [{:path path :name ::map :val data}]
        [{:path path :name name :val data}]))))

(defn- get-spec
  [spec]
  (let [spec (cond
               (satisfies? ISpec spec)
               spec

               (fn? spec)
               (->FnSpec spec nil nil)

               (keyword? spec)
               (get @*registry* spec)

               :else
               (throw (ex-info "unsupported type for spec lookup" {:spec spec})))]
    (when (nil? spec)
      (throw (ex-info "Spec not found" {:spec spec})))
    spec))

(defn defs-impl
  [spec name]
  (cond
    (fn? spec)
    (->FnSpec spec name nil)

    (keyword? spec)
    (let [spec (get-spec spec)]
      (assoc spec :name name))

    (satisfies? ISpec spec)
    (assoc spec :name name)

    :else
    (throw (ex-info "spec can't be resolved" {:spec spec}))))

;; --- Public API

(defn pred
  "Programatically create a spec instance from predicate
  and optionaly a coercer and name."
  ([f c]
   (assoc (get-spec f) :coerce c))
  ([f c n]
   (assoc (get-spec f) :coerce c :name n)))

(defn opt
  [spec]
  (let [spec (get-spec spec)]
    (->OptSpec spec (:name spec))))

(defn &&
  [& specs]
  (let [specs (map get-spec specs)]
    (->AndSpec specs nil)))

(defn dict
  [& keypairs]
  (assert (even? (count keypairs)) "an even number of pairs is mandatory")
  (let [pairs (map (fn [[k s]] [k (get-spec s)]) (partition 2 keypairs))]
    (->MapSpec pairs nil)))

#?(:clj
   (defmacro defs
     [name spec]
     `(swap! *registry* assoc ~name (defs-impl ~spec ~name))))

(defn conform
  [spec data]
  (let [spec (get-spec spec)]
    (-conform spec data)))

(defn explain
  [spec data]
  (let [problems (-explain (get-spec spec) [] data)]
    (if (empty? problems)
      nil
      problems)))

(def ^:const ^:private +uuid-re+
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")

(def ^:const ^:private +email-re+
  #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$")

(defs ::string string?)
(defs ::number number?)
(defs ::keyword keyword?)
(defs ::boolean boolean?)
(defs ::positive pos?)
(defs ::negative neg?)
(defs ::map map?)
(defs ::set set?)
(defs ::coll coll?)
(defs ::vector vector?)

(defs ::uuid #?(:clj #(instance? java.util.UUID %)
                :cljs #(instance? cljs.core.UUID %)))
(defs ::uuid-str
  (pred #(and (string? %) (re-seq +uuid-re+ %))
        #?(:clj #(java.util.UUID/fromString %)
           :cljs #(uuid %))))

(defs ::email #(and (string? %) (re-seq +email-re+ %)))

(defs ::number-str
  (pred #(or (number? %) (and (string? %) (util/numeric? %)))
        #(if (number? %) % (util/parse-number %))))

(defs ::integer-str
  (pred #(or (number? %) (and (string? %) (util/numeric? %)))
        #(if (number? %) (int %) (util/parse-int %))))

(defs ::boolean-str
  (pred #(and (string? %) (re-seq #"^(?:t|true|false|f|0|1)$" %))
        #(contains? #{"t" "true" "1"} %)))

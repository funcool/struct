(ns struct.alpha
  (:refer-clojure :exclude [and])
  (:require [struct.util :as util])
  #?(:cljs (:require-macros struct.alpha)))

(def ^:dynamic *registry* (atom {}))

;; --- Impl

(defprotocol ISpec
  (-conform [_ _])
  (-explain [_ _ _]))

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
  [f c n]
  (assoc (get-spec f) :coerce c :name n))

(defn opt
  [spec]
  (let [spec (get-spec spec)]
    (->OptSpec spec (:name spec))))

(defn and
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
  (-conform (get-spec spec) data))

(defn explain
  [spec data]
  (let [problems (-explain (get-spec spec) [] data)]
    (if (empty? problems)
      nil
      problems)))

;; (defs ::number number?)
;; (defs ::string string?)

;; (defs ::width ::number)
;; (defs ::height ::number)

;; (defs ::even even?)

;; (defs ::size
;;   (map :width (and ::number ::even)
;;        :height ::height
;;        :desc (opt ::string)))
;; ;; )


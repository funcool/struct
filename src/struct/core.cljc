(ns struct.core
  (:refer-clojure :exclude [keyword uuid vector boolean long map set])
  (:require [struct.util :as util])
  #?(:cljs (:require-macros struct.core)))

(def ^:private map' #?(:cljs cljs.core/map
                       :clj clojure.core/map))

(def ^:private vector' #?(:cljs cljs.core/vector
                          :clj clojure.core/vector))
;; --- Impl details

(defn- dissoc-in
  [m [k & ks]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(def ^:private opts-params
  #{:coerce :message :optional :code})

(def ^:private notopts?
  (complement opts-params))

(defn- compile-validator
  [data]
  (cond
    (map? data)
    data

    (fn? data)
    {:code ::custom-predicate
     :optional true
     :validate #(data %2)}

    (vector? data)
    (let [vdata (compile-validator (first data))
          result (split-with notopts? (rest data))
          args (first result)
          opts (apply hash-map (second result))
          ofn  (:validate vdata)
          nfn  (fn [data val]
                 (apply ofn data val args))]
      (merge vdata opts {:validate nfn :args args}))

    :else
    (throw (ex-info (pr-str "Invalid validator data:" data) {:data data}))))

(defn- compile-validation-fn
  [validators]
  (reduce (fn [acc validator]
            (let [validate-fn (:validate validator)
                  optional? (:optional validator)]
              (fn [data value]
                (if (or (and (nil? value) optional?)
                        (validate-fn data value))
                  (acc data value)
                  {:valid? false :validator validator}))))
          (constantly {:valid? true})
          (reverse validators)))

(defn- compile-validation-and-coerce-fn
  [validators]
  (reduce (fn [acc validator]
            (let [validate-fn (:validate validator)
                  optional? (:optional validator)
                  coerce (:coerce validator identity)]
              (fn [data value]
                (cond
                  (and (nil? value) optional?)
                  (acc data value)

                  (validate-fn data value)
                  (update (acc data value) :value coerce)

                  :else
                  {:valid? false :validator validator}))))
          (fn [data value]
            {:valid? true :value value})
          (reverse validators)))

(defn- compile-schema-entry
  [key validators]
  (let [validators (mapv compile-validator validators)]
    {:path (if (vector? key) key [key])
     :vfn (compile-validation-fn validators)
     :cfn (compile-validation-and-coerce-fn validators)}))

(defn- schema-map->vec
  [schema]
  (reduce-kv (fn [acc k v]
               (if (vector? v)
                 (conj acc (cons k v))
                 (conj acc (cons k (list v)))))
             []
             schema))

(defn compile-schema
  [schema]
  (let [entries (cond
                  (vector? schema) (seq schema)
                  (map? schema) (schema-map->vec schema)
                  :else (throw (ex-info "Invalid schema." {})))]
    (reduce (fn [acc [key & validators]]
              (assoc acc key (compile-schema-entry key validators)))
            {::compiled true}
            entries)))

(defn- format-error
  [result value]
  (let [vdata (:validator result)
        msg (:message vdata nil)
        msg (if (fn? msg) (msg vdata) msg)]
    {:code (:code vdata)
     :type (:type vdata)
     :message msg
     :value value}))

(defn- impl-validate
  [schema data]
  (reduce-kv (fn [_ _ {:keys [path vfn] :as item}]
               (let [value (get-in data path)]
                 (or (vfn data value)
                     (reduced false))))
             true
             (dissoc schema ::compiled)))

(defn- impl-validate-and-coerce
  [schema data opts]
  (reduce-kv (fn [acc key {:keys [path vfn cfn] :as entry}]
               (let [value (get-in data path)
                     result (cfn data value)
                     result-value (:value result)]
                 (if (:valid? result)
                   (if (nil? result-value)
                     acc
                     (update acc :data assoc-in path result-value))
                   (let [validator (:validator result)
                         error (format-error result value)]
                     (-> acc
                         (update :data dissoc-in path)
                         (update :errors assoc-in path error))))))
             (if (:strip opts) {:data {}} {:data data})
             (dissoc schema ::compiled)))

(defn- resolve-schema
  [schema]
  (cond
    (delay? schema)
    (resolve-schema @schema)

    (true? (::compiled schema))
    schema

    (or (map? schema)
        (vector? schema))
    (compile-schema schema)

    :else
    (throw (ex-info "Invalid value for schema." {:schema schema}))))

;; --- Public Api

#?(:clj
   (defmacro defs
     [namesym schema]
     {:pre [(symbol? namesym)
            (or (map? schema)
                (vector? schema))]}
     `(def ~namesym
        (delay (compile-schema ~schema)))))

(defn validate
  "Validate data with specified schema.

  This function by default strips all data that are not defined in
  schema, but this behavior can be changed by passing `{:strip false}`
  as third argument."
  ([schema data]
   (validate schema data nil))
  ([schema data opts]
   (let [schema (resolve-schema schema)
         result (impl-validate-and-coerce schema data opts)]
     [(:errors result)
      (:data result)])))

(defn valid?
  [schema data]
  (let [schema (resolve-schema schema)
        result (impl-validate schema data)]
    (:valid? result)))

(defn validate!
  "Analogous function to the `validate` that instead of return
  the errors, just raise a ex-info exception with errors in case
  them are or just return the validated data.

  This function accepts the same parameters as `validate` with
  an additional `:msg` that serves for customize the exception
  message."
  ([schema data]
   (validate! schema data nil))
  ([schema data {:keys [message] :or {message "Schema validation error"} :as opts}]
   (let [[errors data] (validate schema data opts)]
     (if (seq errors)
       (throw (ex-info message errors))
       data))))

;; --- Validators

(def keyword
  {:code ::keyword
   :type ::builtin
   :optional true
   :validate #(keyword? %2)})

(def uuid
  {:code ::uuid
   :type ::builtin
   :optional true
   :validate #?(:clj #(instance? java.util.UUID %2)
                :cljs #(instance? cljs.core.UUID %2))})

(def ^:const ^:private +uuid-re+
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")

(def uuid-str
  {:code ::uuid-str
   :type ::builtin
   :optional true
   :validate #(and (string? %2)
                   (re-seq +uuid-re+ %2))
   :coerce #?(:clj #(java.util.UUID/fromString %)
              :cljs #(uuid %))})

(def email
  (let [rx #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$"]
    {:code ::email
     :type ::builtin
     :optional true
     :validate #(and (string? %2)
                     (re-seq rx %2))}))

(def required
  {:code ::required
   :type ::builtin
   :optional false
   :validate #(if (string? %2)
                (not (empty? %2))
                (not (nil? %2)))})

(def number
  {:code ::number
   :type ::builtin
   :optional true
   :validate #(number? %2)})

(def number-str
  {:code ::number-str
   :type ::builtin
   :optional true
   :validate #(or (number? %2) (and (string? %2) (util/numeric? %2)))
   :coerce #(if (number? %) % (util/parse-number %))})

(def integer
  {:code ::integer
   :type ::builtin
   :optional true
   :validate #?(:cljs #(js/Number.isInteger %2)
                :clj #(integer? %2))})

(def integer-str
  {:code ::integer-str
   :type ::builtin
   :optional true
   :validate #(or (number? %2) (and (string? %2) (util/numeric? %2)))
   :coerce #(if (number? %) (int %) (util/parse-int %))})

(def boolean
  {:code ::boolean
   :type ::builtin
   :optional true
   :validate #(or (= false %2) (= true %2))})

(def boolean-str
  {:code ::boolean-str
   :type ::builtin
   :optional true
   :validate #(and (string? %2)
                   (re-seq #"^(?:t|true|false|f|0|1)$" %2))
   :coerce #(contains? #{"t" "true" "1"} %)})

(def string
  {:code ::string
   :type ::builtin
   :optional true
   :validate #(string? %2)})

(def string-like
  {:code ::string-like
   :type ::builtin
   :optional true
   :validate (constantly true)
   :coerce str})

(def in-range
  {:code ::in-range
   :type ::builtin
   :optional true
   :validate #(and (number? %2)
                   (number? %3)
                   (number? %4)
                   (<= %3 %2 %4))})

(def positive
  {:code ::positive
   :type ::builtin
   :optional true
   :validate #(pos? %2)})

(def negative
  {:code ::negative
   :type ::builtin
   :optional true
   :validate #(neg? %)})

(def map
  {:code ::map
   :type ::builtin
   :optional true
   :validate #(map? %2)})

(def set
  {:code ::set
   :type ::builtin
   :optional true
   :validate #(set? %2)})

(def coll
  {:code ::coll
   :type ::builtin
   :optional true
   :validate #(coll? %2)})

(def vector
  {:code ::vector
   :type ::builtin
   :optional true
   :validate #(vector? %2)})

(def every
  {:code ::every
   :type ::builtin
   :optional true
   :validate #(every? %3 %2)})

(def member
  {:code ::member
   :type ::builtin
   :optional true
   :validate #(some #{%2} %3)})

(def function
  {:code ::function
   :type ::builtin
   :optional true
   :validate #(fn? %2)})

(def identical-to
  {:code ::identical-to
   :type ::builtin
   :optional true
   :validate (fn [state v ref]
               (let [prev (get state ref)]
                 (= prev v)))})

(def min-count
  (letfn [(validate [_ v minimum]
            {:pre [(number? minimum)]}
            (>= (count v) minimum))]
    {:code ::min-count
     :type ::builtin
     :optional true
     :validate validate}))

(def max-count
  (letfn [(validate [_ v maximum]
            {:pre [(number? maximum)]}
            (<= (count v) maximum))]
    {:code ::max-count
     :type ::builtin
     :optional true
     :validate validate}))

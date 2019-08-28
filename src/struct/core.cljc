(ns struct.core
  (:refer-clojure :exclude [keyword uuid vector boolean long map set])
  (:require [struct.util :as util])
  #?(:cljs (:require-macros struct.core)))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))


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
  #{:coerce :message :optional :type})

(def ^:private notopts?
  (complement opts-params))

(defn- compile-validator
  [data]
  (cond
    (map? data)
    data

    (fn? data)
    {:type ::custom-predicate
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
      (merge vdata opts {:validate nfn}))

    :else
    (throw (ex-info (pr-str "Invalid validator data:" data) {:data data}))))

(defn compile-validation-fn
  [items]
  (reduce (fn [acc item]
            (let [validate-fn (:validate item)
                  optional? (:optional item)]
              (fn [data value]
                (if (or (and (nil? value) optional?)
                        (validate-fn data value))
                  (acc data value)
                  {:valid? false :validator item}))))
          (constantly {:valid? true})
          (reverse items)))

(defn- compile-coerce-fn
  [items]
  (reduce (fn [acc item]
            (let [coerce (:coerce item identity)]
              #(coerce (acc %))))
          identity
          (reverse items)))

(defn- compile-schema-entry
  [[key & validators]]
  (let [validators (mapv compile-validator validators)]
    {:path (if (vector? key) key [key])
     :vfn (compile-validation-fn validators)
     :cfn (compile-coerce-fn validators)}))

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
  (let [items (cond
                (vector? schema) (seq schema)
                (map? schema) (schema-map->vec schema)
                :else (throw (ex-info "Invalid schema." {})))]
    {::schema true
     ::items (mapv compile-schema-entry items)}))

(defn- format-error
  [result value]
  (let [vdata (:validator result)
        msg (:message vdata nil)
        msg (if (fn? msg) (msg vdata) msg)]
    {:type (:type vdata)
     :message msg
     :value value}))

(defn- impl-validate
  [data items]
  (reduce (fn [_ {:keys [path vfn] :as item}]
            (let [value (get-in data path)]
              (or (vfn data value)
                  (reduced false))))
          true
          items))

(defn- impl-validate-and-coerce
  [data items opts]
  (reduce (fn [acc {:keys [path vfn cfn] :as item}]
            (let [value (get-in data path)
                  result (vfn data value)]
              (if (:valid? result)
                (let [val (cfn value)]
                  (if (nil? val)
                    acc
                    (update acc :data assoc-in path val)))
                (let [validator (:validator result)
                      error (format-error result value)]
                  (-> acc
                      (update :data dissoc-in path)
                      (update :errors assoc-in path error))))))
          (if (:strip opts) {:data {}} {:data data})
          items))

(defn- resolve-schema
  [schema]
  (cond
    (delay? schema)
    (resolve-schema @schema)

    (true? (::schema schema))
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
  ([data schema]
   (validate data schema nil))
  ([data schema opts]
   (let [schema (resolve-schema schema)
         result (impl-validate-and-coerce data (::items schema) opts)]
     [(:errors result)
      (:data result)])))

(defn valid?
  [data schema]
  (let [schema (resolve-schema schema)
        result (impl-validate data (::items schema))]
    (:valid? result)))

(defn validate!
  "Analogous function to the `validate` that instead of return
  the errors, just raise a ex-info exception with errors in case
  them are or just return the validated data.

  This function accepts the same parameters as `validate` with
  an additional `:msg` that serves for customize the exception
  message."
  ([data schema]
   (validate! data schema nil))
  ([data schema {:keys [message] :or {message "Schema validation error"} :as opts}]
   (let [[errors data] (validate data schema opts)]
     (if (seq errors)
       (throw (ex-info message errors))
       data))))

;; --- Validators

(def keyword
  {:type ::keyword
   :optional true
   :validate #(keyword? %2)})

(def uuid
  {:type ::uuid
   :optional true
   :validate #?(:clj #(instance? java.util.UUID %2)
                :cljs #(instance? cljs.core.UUID %2))})

(def ^:const ^:private +uuid-re+
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")

(def uuid-str
  {:type ::uuid-str
   :optional true
   :validate #(and (string? %2)
                   (re-seq +uuid-re+ %2))
   :coerce #?(:clj #(java.util.UUID/fromString %)
              :cljs #(uuid %))})

(def email
  (let [rx #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$"]
    {:type ::email
     :optional true
     :validate #(and (string? %2)
                     (re-seq rx %2))}))

(def required
  {:type ::required
   :optional false
   :validate #(if (string? %2)
                (not (empty? %2))
                (not (nil? %2)))})

(def number
  {:type ::number
   :optional true
   :validate #(number? %2)})

(def number-str
  {:type ::number-str
   :optional true
   :validate #(or (number? %2) (and (string? %2) (util/numeric? %2)))
   :coerce #(if (number? %) % (util/parse-number %))})

(def integer
  {:type ::integer
   :optional true
   :validate #?(:cljs #(js/Number.isInteger %2)
                :clj #(integer? %2))})

(def integer-str
  {:type ::integer-str
   :optional true
   :validate #(or (number? %2) (and (string? %2) (util/numeric? %2)))
   :coerce #(if (number? %) (int %) (util/parse-int %))})

(def boolean
  {:type ::boolean
   :optional true
   :validate #(or (= false %2) (= true %2))})

(def boolean-str
  {:type ::boolean-str
   :optional true
   :validate #(and (string? %2)
                   (re-seq #"^(?:t|true|false|f|0|1)$" %2))
   :coerce #(contains? #{"t" "true" "1"} %)})

(def string
  {:type ::string
   :optional true
   :validate #(string? %2)})

(def string-like
  {:type ::string-like
   :optional true
   :validate (constantly true)
   :coerce str})

(def in-range
  {:type ::in-range
   :optional true
   :validate #(and (number? %2)
                   (number? %3)
                   (number? %4)
                   (<= %3 %2 %4))})

(def positive
  {:type ::positive
   :optional true
   :validate #(pos? %2)})

(def negative
  {:type ::negative
   :optional true
   :validate #(neg? %)})

(def map
  {:type ::map
   :optional true
   :validate #(map? %2)})

(def set
  {:type ::set
   :optional true
   :validate #(set? %2)})

(def coll
  {:type ::coll
   :optional true
   :validate #(coll? %2)})

(def vector
  {:type ::vector
   :optional true
   :validate #(vector? %2)})

(def every
  {:type ::every
   :optional true
   :validate #(every? %3 %2)})

(def member
  {:type ::member
   :optional true
   :validate #(some #{%2} %3)})

(def function
  {:type ::function
   :optional true
   :validate #(fn? %2)})

(def identical-to
  {:type ::identical-to
   :optional true
   :validate (fn [state v ref]
               (let [prev (get state ref)]
                 (= prev v)))})

(def min-count
  (letfn [(validate [_ v minimum]
            {:pre [(number? minimum)]}
            (>= (count v) minimum))]
    {:type ::min-count
     :optional true
     :validate validate}))

(def max-count
  (letfn [(validate [_ v maximum]
            {:pre [(number? maximum)]}
            (<= (count v) maximum))]
    {:type ::max-count
     :optional true
     :validate validate}))

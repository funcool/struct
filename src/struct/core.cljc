(ns struct.core
  (:refer-clojure :exclude [keyword uuid vector boolean long map set])
  (:require [cuerdas.core :as str]))

;; --- Impl details

(def ^:private map' #?(:cljs cljs.core/map
                       :clj clojure.core/map))

(defn- apply-validation
  [step data value]
  (if-let [validate (:validate step nil)]
    (let [args (:args step [])]
      (if (:state step)
        (apply validate data value args)
        (apply validate value args)))
    true))

(defn- dissoc-in
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn- prepare-message
  [opts step]
  (if (::nomsg opts)
    ::nomsg
    (let [msg (:message step "errors.invalid")
          tr (:translate opts identity)]
      (apply str/format (tr msg) (:args step)))))

(def ^:const ^:private opts-params
  #{:coerce :message :optional})

(def ^:private notopts?
  (complement opts-params))

(defn- build-step
  [key item]
  (if (vector? item)
    (let [validator (first item)
          result (split-with notopts? (rest item))
          args (first result)
          opts (apply hash-map (second result))]
      (merge (assoc validator :args args :path [key])
             (select-keys opts [:coerce :message :optional])))
    (assoc item :args [] :path (if (vector? key) key [key]))))

(defn- normalize-step-map-entry
  [acc key value]
  (if (vector? value)
    (reduce #(conj! %1 (build-step key %2)) acc value)
    (conj! acc (build-step key value))))

(defn- normalize-step-entry
  [acc [key & values]]
  (reduce #(conj! %1 (build-step key %2)) acc values))

(defn- build-steps
  [schema]
  (cond
    (vector? schema)
    (persistent!
     (reduce normalize-step-entry (transient []) schema))

    (map? schema)
    (persistent!
     (reduce-kv normalize-step-map-entry (transient []) schema))

    :else
    (throw (ex-info "Invalid schema." {}))))

(defn- strip-values
  [data steps]
  (reduce (fn [acc path]
            (let [value (get-in data path ::notexists)]
              (if (not= value ::notexists)
                (assoc-in acc path value)
                acc)))
          {}
          (into #{} (map' :path steps))))

(defn- validate-internal
  [data steps opts]
  (loop [skip #{}
         errors nil
         data data
         steps steps]
    (if-let [step (first steps)]
      (let [path (:path step)
            value (get-in data path)]
        (cond
          (contains? skip path)
          (recur skip errors data (rest steps))

          (and (nil? value) (:optional step))
          (recur skip errors data (rest steps))

          (apply-validation step data value)
          (let [value ((:coerce step identity) value)]
            (recur skip errors (assoc-in data path value) (rest steps)))

          :else
          (let [message (prepare-message opts step)]
            (recur (conj skip path)
                   (assoc-in errors path message)
                   (dissoc-in data path)
                   (rest steps)))))
      [errors data])))

;; --- Public Api

(defn validate
  "Validate data with specified schema.

  This function by default strips all data that does not defined in
  schema, but this behavior can be changed passing `{:strip false}`
  as third argument."
  ([data schema]
   (validate data schema nil))
  ([data schema {:keys [strip]
                 :or {strip false}
                 :as opts}]
   (let [steps (build-steps schema)
         data (if strip (strip-values data steps) data)]
     (validate-internal data steps opts))))

(defn validate-single
  "A helper that used just for validate one value."
  ([data schema] (validate-single data schema nil))
  ([data schema opts]
   (let [data {:field data}
         steps (build-steps {:field schema})]
     (mapv :field (validate-internal data steps opts)))))

(defn validate!
  "Analogous function to the `validate` that instead of return
  the errors, just raise a ex-info exception with errors in case
  them are or just return the validated data.

  This function accepts the same parameters as `validate` with
  an additional `:message` that serves for customize the exception
  message."
  ([data schema]
   (validate! data schema nil))
  ([data schema {:keys [message] :or {message "Schema validation error"} :as opts}]
   (let [[errors data] (validate data schema opts)]
     (if (seq errors)
       (throw (ex-info message errors))
       data))))

(defn valid?
  "Return true if the data matches the schema, otherwise
  return false."
  [data schema]
  (nil? (first (validate data schema {::nomsg true}))))

(defn valid-single?
  "Analogous function to `valid?` that just validates single value."
  [data schema]
  (nil? (first (validate-single data schema {::nomsg true}))))

;; --- Validators

(def keyword
  {:message "must be a keyword"
   :optional true
   :validate keyword?
   :coerce identity})

(def uuid
  {:message "must be an uuid"
   :optional true
   :validate #?(:clj #(instance? java.util.UUID %)
                :cljs #(instance? cljs.core.UUID %))})

(def ^:const ^:private +uuid-re+
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")

(def uuid-str
  {:message "must be an uuid"
   :optional true
   :validate #(and (string? %)
                   (re-seq +uuid-re+ %))
   :coerce #?(:clj #(java.util.UUID/fromString %)
              :cljs #(uuid %))})

(def email
  (let [rx #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$"]
    {:message "must be a valid email"
     :optional true
     :validate #(and (string? %)
                     (re-seq rx %))}))

(def required
  {:message "this field is mandatory"
   :optional false
   :validate #(if (string? %)
                 (not (empty? %))
                 (not (nil? %)))})

(def number
  {:message "must be a number"
   :optional true
   :validate number?})

(def number-str
  {:message "must be a number"
   :optional true
   :validate #(and (string? %)
                   (re-seq #"^[-+]?[0-9]*\.?[0-9]+$" %))
   :coerce #?(:cljs #(js/parseFloat %)
              :clj #(Double/parseDouble %))})

(def integer
  (letfn [(validate [v]
            #?(:cljs (js/Number.isInteger v)
               :clj (integer? v)))]
    {:message "must be a integer"
     :optional true
     :validate validate}))

(def integer-str
  (letfn [(coerce [v]
            #?(:clj (Long/parseLong v)
               :cljs (let [result (js/parseInt v 10)]
                       (if (js/isNaN result) v result))))
          (validate [v]
            (and (string? v)
                 (re-seq #"^[-+]?\d+$" v)))]
    {:message "must be a long"
     :optional true
     :validate validate
     :coerce coerce}))

(def boolean
  {:message "must be a boolean"
   :optional true
   :validate #(or (= false %) (= true %))})

(def boolean-str
  (letfn [(validate [v]
            (and (string? v)
                 (re-seq #"^(?:t|true|false|f|0|1)$" v)))
          (coerce [v]
            (contains? #{"t" "true" "1"} v))]
    {:message "must be a boolean"
     :optional true
     :validate validate
     :coerce coerce}))

(def string
  {:message "must be a string"
   :optional true
   :validate string?})

(def string-like
  {:message "must be a string"
   :optional true
   :coerce str})

(def in-range
  (letfn [(validate [v from to]
            {:pre [(number? from) (number? to)]}
            (and (number? v)
                 (<= from v to)))]
    {:message "not in range"
     :optional true
     :validate validate}))

(def positive
  {:message "must be positive"
   :optional true
   :validate pos?})

(def negative
  {:message "must be negative"
   :optional true
   :validate neg?})

(def map
  {:message "must be a map"
   :optional true
   :validate map?})

(def set
  {:message "must be a set"
   :optional true
   :validate set?})

(def coll
  {:message "must be a collection"
   :optional true
   :validate coll?})

(def vector
  {:message "must be a vector instance"
   :optional true
   :validate vector?})

(def every
  {:message "must match the predicate"
   :optional true
   :validate #(every? %2 %1)})

(def member
  {:message "not in coll"
   :optional true
   :validate #(some #{%1} %2)})

(def function
  {:message "must be a function"
   :optional true
   :validate ifn?})

(def identical-to
  {:message "does not match"
   :optional true
   :state true
   :validate (fn [state v ref]
               (let [prev (get state ref)]
                 (= prev v)))})

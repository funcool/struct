(ns struct.core
  (:refer-clojure :exclude [keyword uuid vector boolean long map set]))

;; --- Impl details

(def ^:private map' #?(:cljs cljs.core/map
                       :clj clojure.core/map))

(defn- apply-coersion
  [step value]
  (if-let [coercefn (:coerce step identity)]
    (try
      [nil (coercefn value)]
      (catch #?(:cljs :default :clj Exception) e
        [e value]))
    value))

(defn- apply-validation
  [step value]
  (if-let [validationfn (:validate step nil)]
    (let [args (:args step [])]
      (apply validationfn value args))
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

(defn- run-step
  [[errors data] step]
  (let [path (:path step)
        value (get-in data path)]
    (if (and (nil? value) (:optional step))
      [errors data]
      (let [message (:message step "invalid")]
        (if (apply-validation step value)
          (let [[err value] (apply-coersion step value)]
            (if err
              [(update-in errors path conj message)
               (dissoc-in data path)]
              [errors (assoc-in data path value)]))
          [(update-in errors path conj message)
           (dissoc-in data path)])))))

(defn- build-step
  [key item]
  (if (vector? item)
    (let [validator (first item)
          result (split-with (complement keyword?) (rest item))
          args (first result)
          opts (apply hash-map (second result))]
      (merge (assoc validator :args args :path [key])
             (select-keys opts [:coerce :message :optional])))
    (assoc item :args [] :path [key])))

(defn- normalize-step-map-entry
  [acc key value]
  (if (vector? value)
    (reduce #(conj %1 (build-step key %2)) acc value)
    (conj acc (build-step key value))))

(defn- normalize-step-entry
  [acc [key & values]]
  (reduce #(conj %1 (build-step key %2)) acc values))

(defn- build-steps
  [schema]
  (cond
    (vector? schema)
    (reduce normalize-step-entry [] schema)

    (map? schema)
    (reduce-kv normalize-step-map-entry [] schema)

    :else
    (throw (ex-info "Invalid schema." {}))))

(defn- strip-values
  [data steps]
  (reduce (fn [acc path]
            (if-let [value (get-in data path)]
              (assoc-in acc path value)
              acc))
          {}
          (into #{} (map' :path steps))))

;; --- Public Api

(defn validate
  ([data schema] (validate data schema nil))
  ([data schema {:keys [strip] :or {strip true}}]
   (let [steps (build-steps schema)
         data  (if strip (strip-values data steps) data)
         seed [nil data]]
     (reduce run-step seed steps))))

(defn validate!
  ([data schema]
   (validate! data schema nil))
  ([data schema {:keys [message] :or {message "Schema validation error"} :as opts}]
   (let [[errors data] (validate data schema opts)]
     (if (seq errors)
       (throw (ex-info message errors))
       data))))

(defn valid?
  [data schema]
  (empty? (first (validate data schema))))

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
   :validato #(every? %2 %1)})

(def member
  {:message "not in coll"
   :optional true
   :validate #(some #{%1} %2)})

(def function
  {:message "must be a function"
   :optional true
   :validate ifn?})


(ns struct.core
  (:refer-clojure :exclude [keyword uuid vector boolean long]))

;; --- Impl details

(defn- apply-coersion
  [step value]
  (if-let [coercefn (:coerce step identity)]
    (try
      (coercefn value)
      (catch #?(:cljs :default :clj Exception) e
        value))
    value))

(defn- apply-validation
  [step value]
  (if-let [validationfn (:validate step nil)]
    (let [args (:args step [])]
      (apply validationfn value args))
    true))

(defn dissoc-in
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
          (let [value (apply-coersion step value)]
            [errors (assoc-in data path value)])
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

(defn- normalize-step
  [acc key value]
  (if (vector? value)
    (reduce #(conj %1 (build-step key %2)) acc value)
    (conj acc (build-step key value))))

(defn- build-steps
  [schema]
  (reduce-kv normalize-step [] schema))

;; --- Public Api

(defn validate
  [data schema]
  (let [steps (build-steps schema)
        seed [nil data]]
    (reduce run-step seed steps)))

(defn validate!
  ([data schema]
   (validate! data schema "Schema validation."))
  ([data schema message]
   (if-let [[errors data] (validate data schema)]
     (throw (ex-info message errors))
     data)))

(defn valid?
  [data schema]
  (empty? (first (validate data schema))))

;; --- Validators

(def keyword
  {:message "must be a keyword instance"
   :optional true
   :validate keyword?
   :coerce identity})

(def uuid
  {:message "must be an uuid instance"
   :optional true
   :validate #?(:clj #(instance? java.util.UUID %)
                :cljs #(instance? cljs.core.UUID %))})

(def ^:const ^:private +uuid-re+
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")

(def uuid-like
  {:message "must be an uuid"
   :optional true
   :validate #(and (string? %)
                   (re-seq +uuid-re+ %))
   :coerce #?(:clj #(java.util.UUID/fromString %)
              :cljs #(uuid %))})

(def vector
  {:message "must be a vector instance"
   :optional true
   :validate vector?})

(def function
  {:message "must be a function"
   :optional true
   :validate ifn?})

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

(def number-like
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

(def integer-like
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

(def boolean-like
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




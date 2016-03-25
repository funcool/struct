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

(defn- run-step
  [[errors data] step]
  (let [path (:path step)
        value (get-in data path)]
    (if (and (nil? value) (:optional step))
      [errors data]
      (let [value (apply-coersion step value)
            message (:message step "invalid")]
        (if (apply-validation step value)
          [errors (assoc-in data path value)]
          [(update-in errors path conj message)
           (assoc-in data path value)])))))

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
  {:message "must be a uuid instance"
   :optional true
   :validate #?(:clj #(instance? java.util.UUID %)
                :cljs #(instance? cljs.core.UUID %))})

(def vector
  {:message "must be a vector instance"
   :optional true
   :validate vector?})

(def function
  {:message "must be a function"
   :optional true
   :validate ifn?})

(def ^:const ^:private +email-re+
  #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$")

(def email
  {:message "must be a valid email"
   :optional true
   :validate #?(:clj #(clojure.core/boolean (re-seq +email-re+ %))
                 :cljs #(cljs.core/boolean (re-seq +email-re+ %)))})

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

(def integer
  {:message "must be a integer"
   :optional true
   :validate integer?})

(def long
  (letfn [(coerce [v]
            (if (string? v)
              #?(:clj (Long/parseLong v)
                 :cljs (let [result (js/parseInt v 10)]
                         (if (js/isNaN result) v result)))
              v))
          (validate [v]
            #?(:clj (instance? Long v)
               :cljs (js/Number.isInteger v)))]
    {:message "must be a long"
     :optional true
     :validate validate
     :coerce coerce}))

(def boolean
  {:message "must be a boolean"
   :optional true
   :validate #(or (= false %) (= true %))})

(def string
  {:message "must be a string"
   :optional true
   :validate string?})




(ns struct.core
  (:refer-clojure :exclude [keyword uuid vector boolean]))

;; --- Impl details

(defprotocol IValidator
  (-validate [_ path value context]))

(defn- call-coerce
  [func value]
  (if (nil? value)
    value
    (try
      (func value)
      (catch #?(:cljs :default :clj Exception) e
        value))))

(defrecord Validator [optional message args validator coerce]
  IValidator
  (-validate [_ path value context]
    (if (and (nil? value) optional)
      context
      (let [value (call-coerce coerce value)
            [errors state] context]
        (if (apply validator value args)
          [errors (assoc-in state path value)]
          [(update-in errors path conj message)
           (assoc-in state path value)])))))

(defn- build-validator
  [item]
  (if (vector? item)
    (let [validator (first item)
          result (split-with (complement keyword?) (rest item))
          args (first result)
          opts (apply hash-map (second result))]
      (merge (assoc validator :args args)
             (select-keys opts [:coerce :message :optional])))
    item))

(defn- normalize-schema-entry
  [acc key value]
  (if (vector? value)
    (reduce #(conj %1 [[key] (build-validator %2)]) acc value)
    (conj acc [[key] (build-validator value)])))

(defn- build-steps
  [schema]
  (reduce-kv normalize-schema-entry [] schema))

;; --- Public Api

(defn validate
  [data schema]
  (let [steps (build-steps schema)
        seed [nil data]]
    (reduce (fn [context [path v]]
              (let [value (-> (second context)
                              (get-in path))]
                (-validate v path value context)))
            seed
            steps)))

(defn validate!
  ([data schema]
   (validate! data schema "Schema validation."))
  ([data schema message]
   (if-let [[errors data] (validate data schema)]
     (throw (ex-info message errors))
     data)))

(defn validator
  "A helper for define a validator."
  [& {:keys [message validator coerce optional]
      :or {message "invalid" coerce identity optional true}}]
  {:pre [(ifn? validator)]}
  (map->Validator {:optional optional
                   :message message
                   :validator validator
                   :coerce coerce
                   :args []}))

;; --- Validators

(def keyword
  (validator
   :message "must be a keyword instance"
   :validator keyword?
   :coerce identity))

(def uuid
  (validator
   :message "must be a uuid instance"
   :validator #?(:clj #(instance? java.util.UUID %)
                 :cljs #(instance? cljs.core.UUID %))))

(def vector
  (validator
   :message "must be a vector instance"
   :validator vector?))

(def function
  (validator
   :message "must be a function"
   :validator ifn?))

(def ^:const ^:private +email-re+
  #"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$")

(def email
  (validator
   :message "must be a valid email"
   :validator #?(:clj #(clojure.core/boolean (re-seq +email-re+ %))
                 :cljs #(cljs.core/boolean (re-seq +email-re+ %)))))

(def required
  (validator
   :optional false
   :message "this field is mandatory"
   :validator #(if (string? %)
                 (not (empty? %))
                 (not (nil? %)))))

(def number
  (validator
   :message "must be a number"
   :validator number?))

(def integer
  (validator
   :message "must be a integer"
   :validator integer?))

(def boolean
  (validator
   :message "must be a boolean"
   :validator #(or (= false %) (= true %))))

(def string
  (validator
   :message "must be a string"
   :validator string?))




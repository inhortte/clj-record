(ns clj-record.core
  (:require [clojure.contrib.sql        :as sql]
            [clojure.contrib.str-utils  :as str-utils])
  (:use (clj-record meta util callbacks)))

(defn table-name
  "Retrieves table-name from model-metadata."
  [model-name]
  (model-metadata-for model-name :table-name))

(defn set-table-name
  "Puts table-name into model metadata."
  [model-name tbl-name]
  (dosync (set-model-metadata-for model-name :table-name tbl-name)))

(defn set-db-spec [model-name db-spec]
  (dosync (set-model-metadata-for model-name :db-spec db-spec)))

(defn to-conditions
  "Converts the given attribute map into a clojure.contrib.sql style 'where-params,'
  a vector containing a parameterized conditions string followed by ordered values for the parameters.
  Conditions will be ANDed together.
  Nil attributes will be turned into 'attr_name IS NULL' with no value in the vector."
  [attributes]
  ; XXX: Surely there's a better way.
  (let [[parameterized-conditions values] (reduce
      (fn [[parameterized-conditions values] [attribute value]]
       (cond
         (nil? value)
         [(conj parameterized-conditions (format "%s IS NULL" (name attribute))) values]
         (fn? value)
         (let [[new-condition new-values] (value attribute)]
           [(conj parameterized-conditions new-condition) (apply conj values new-values)])
         :else
         [(conj parameterized-conditions (format "%s = ?" (name attribute))) (conj values value)]))
      [[] []]
      attributes)]
    (apply vector (str-utils/str-join " AND " parameterized-conditions) values)))

(defn parse-order-condition
  "Accepts either a keyword, or a hash-map containing a single entry.
Both the key and the value of the hash-map should be keywords. The value of the
hash-map should be either :asc or :desc. Returns a space-separated string of all
the keywords.

Throws IllegalArgumentException if the hash-map doesn't have exactly 1 element."
  [value]
  (cond
    (keyword? value) (name value)
    (map? value)
    (if (not= (count value) 1)
      (throw (IllegalArgumentException.
              "Hash-map must contain exactly one element."))
      (format "%s %s"
              (name (first (keys value)))
              (name (first (vals value)))))))

(defn order-conditions
  "If given a vector, returns a comma and space separated string
of parsed order conditions of each element. Otherwise, the entire
input is parsed as an order condition."
  [attributes]
  (if (vector? attributes)
    (str-utils/str-join ", " (map parse-order-condition attributes))
    (parse-order-condition attributes)))

(defmacro connected
  "Ensures that the body is run with a single DB connection.
  Doesn't create a new connection if there already is one.
  You're probably more interested in the 'transaction' macro."
  [db-spec & body]
  `(let [func# (fn [] ~@body)]
    (if (sql/find-connection)
      (func#)
      (sql/with-connection ~db-spec (func#)))))

(defmacro transaction
  "Runs body in a single DB transaction, first ensuring there's a connection."
  [db-spec & body]
  `(connected ~db-spec
    (sql/transaction
      ~@body)))

(defn insert
  "Inserts a record populated with attributes and returns the generated id."
  [model-name attributes]
  (transaction (db-spec-for model-name)
    (let [attributes (run-callbacks attributes model-name :before-save)]
      (sql/insert-values (table-name model-name) (keys attributes) (vals attributes)))
    (sql/with-query-results rows [(id-query-for (db-spec-for model-name) (table-name model-name))]
      (val (first (first rows))))))

(defn parse-query-options
  "Accepts a sequence of an optional map or vector optionally
followed by a sequence of keyword and value option pairs.
Returns a map of parsed options and query params."
  [params-and-options]
  (let [contains-params? (not (keyword? (first params-and-options)))
        options (apply hash-map (if contains-params?
                                  (rest params-and-options)
                                  params-and-options))
        attributes-or-where-params (if contains-params?
                                     (first params-and-options))
        [parameterized-where & values]
        (if (map? attributes-or-where-params)
          (to-conditions attributes-or-where-params)
          attributes-or-where-params)]
    (merge options {:where parameterized-where :values values})))

(defn create-query
  "Returns a SQL SELECT statement for the given model using provided options."
  [model-name params-and-options]
  (let [options (parse-query-options params-and-options)
        query-string
        (str-utils/str-join
         " "
         [(format "select * from %s" (table-name model-name))
          (if (:where options)
            (format "where %s" (:where options)))
          (if (:order options)
            (format "order by %s" (order-conditions (:order options))))
          (if (:limit options)
            (format "limit %s" (:limit options)))])]
    (apply vector query-string (:values options))))

(defn find-by-sql
  "Returns a vector of matching records.
  select-query-and-values should be something like
    [\"SELECT id, name FROM manufacturers WHERE id = ?\" 23]
  This allows the caller total control over the SELECT and FROM clauses, but note that callbacks are still run,
  so if you omit columns your callbacks will have to be written to tolerate incomplete records."
  [model-name select-query-and-values]
    (connected (db-spec-for model-name)
      (sql/with-query-results rows select-query-and-values
        (doall (map #(with-meta
                       (run-callbacks (merge {} %) model-name :after-load)
                       {:model (keyword model-name)}) rows)))))

(defn find-records
  "Returns a vector of matching records.
  Given a where-params vector, uses it as-is. (See clojure.contrib.sql/with-query-results.)
  Given a map of attribute-value pairs, uses to-conditions to convert to where-params.
  Parses all additional keyword-value pairs as query options."
  [model-name & params-and-options]
  (let [select-query (create-query model-name params-and-options)]
    (find-by-sql model-name select-query)))

(defn get-record
  "Retrieves record by id, returns nil if not found."
  [model-name id]
  (first (find-records model-name {:id id})))

(defn create
  "Inserts a record populated with attributes and returns it."
  [model-name attributes]
  (let [id (insert model-name attributes)]
    (connected (db-spec-for model-name)
      (get-record model-name id))))

(defn update
  "Updates by (partial-record :id), updating only those columns included in partial-record."
  [model-name partial-record]
  (connected (db-spec-for model-name)
    (let [id (partial-record :id)
          partial-record (-> partial-record (run-callbacks model-name :before-save :before-update) (dissoc :id))]
      (sql/update-values (table-name model-name) ["id = ?" id] partial-record)
      (assoc partial-record :id id))))

(defn destroy-record
  "Deletes by (record :id)."
  [model-name record]
  (connected (db-spec-for model-name)
    (sql/delete-rows (table-name model-name) ["id = ?" (:id record)])))

(defn destroy-records
  "Deletes all records matching (-> attributes to-conditions)."
  [model-name attributes]
  (connected (db-spec-for model-name)
    (sql/delete-rows (table-name model-name) (to-conditions attributes))))

(defn- defs-from-option-groups [model-name option-groups]
  (reduce
    (fn [def-forms [option-group-name & options]]
      (let [option-ns (symbol (str "clj-record." (name option-group-name)))
            expand-init-option-fn (ns-resolve option-ns 'expand-init-option)]
        (if (nil? expand-init-option-fn)
          (throw (IllegalArgumentException. (format "%s/expand-init-option not defined" option-ns))))
        (into def-forms (map #(apply expand-init-option-fn model-name %) options))))
    []
    option-groups))

(defn- split-out-init-options [init-options]
  (loop [top-level-options {}
         remaining-options init-options]
    (if (keyword? (first remaining-options))
      (recur
        (assoc top-level-options (first remaining-options) (fnext remaining-options))
        (nnext remaining-options))
      [top-level-options remaining-options])))

(defmacro init-model
  "Macro to turn a namespace into a 'model.'
  The segment of the namespace name following the last dot is used as the model-name.
  Model-specific versions of most public functions in clj-record.core are defined 
  in the model namespace (where the model-name as first argument can be omitted).
  Optional forms for associations and validation are specified here.
  See clj_record/test/model/manufacturer.clj for an example."
  [& init-options]
  (let [model-name (last (str-utils/re-split #"\." (name (ns-name *ns*))))
        [top-level-options option-groups] (split-out-init-options init-options)
        tbl-name (or (top-level-options :table-name) (pluralize model-name))
        optional-defs (defs-from-option-groups model-name option-groups)]
    `(do
      (init-model-metadata ~model-name)
      (set-db-spec ~model-name ~'db)
      (set-table-name ~model-name ~tbl-name)
      (def ~'table-name (table-name ~model-name))
      (defn ~'model-metadata [& args#]
        (apply model-metadata-for ~model-name args#))
      (defn ~'table-name [] (table-name ~model-name))
      (defn ~'get-record [id#]
        (get-record ~model-name id#))
      (defn ~'find-records [& attributes#]
        (apply find-records ~model-name attributes#))
      (defn ~'find-by-sql [select-query-and-values#]
        (find-by-sql ~model-name select-query-and-values#))
      (defn ~'create [attributes#]
        (create ~model-name attributes#))
      (defn ~'insert [attributes#]
        (insert ~model-name attributes#))
      (defn ~'update [attributes#]
        (update ~model-name attributes#))
      (defn ~'destroy-record [record#]
        (destroy-record ~model-name record#))
      (defn ~'validate [record#]
        (clj-record.validation/validate ~model-name record#))
      ~@optional-defs)))

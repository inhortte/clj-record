(ns clj-record.query
  (:require [clojure.contrib.str-utils :as str-utils]))

(defn- operator-fn
  ([operator-format values] (operator-fn operator-format values nil))
  ([operator-format values join-with]
    (if (some nil? values) (throw (IllegalArgumentException. "A query argument must not be nil."))) ; Do we really want this?
    (fn [attribute]
      (let [clause-params-vector
            (reduce (fn [operator-params value]
                      (conj operator-params "?"))
                    [] values)
            clause-params
            (str-utils/str-join join-with clause-params-vector)]
        [(format (str "%s " operator-format)
                 (name attribute)
                 clause-params)
         values]))))


(comment

  (find-records
   (q/and-conditions
    {:id id}
    (q/or-conditions
     {:privacy false}
     {:user 7}
     (q/and-conditions
      {:shared true}
      {:shared-user 7}))))

"WHERE `id` = ? AND (`privacy` = ? OR `user` = ? OR (`shared` = ? AND `shared-user` = ?))"
[4 false 1 true 1])

(defmulti parse-condition type)

(defn combine-conditions
  [separator & conditions]
  (let [parsed-conditions (map parse-condition conditions)]
    [(str "("
          (str-utils/str-join separator (map first parsed-conditions))
          ")")
     (reduce #(into %1 %2) (map second parsed-conditions))]))

(defn and-conditions
  [& conditions]
  (apply combine-conditions " AND " conditions))

(defn or-conditions
  [& conditions]
  (apply combine-conditions " OR " conditions))

(defn backquote
  [s]
  (format "%s" s))

(defn parse-symbol
  [s]
  (cond
    (keyword? s) (backquote (name s))
    (string? s) (backquote s)
    :else s))

(defn equal
  ([value]
     (operator-fn "= %s" [value]))
  ([op1 op2]
     (let [attr (parse-symbol op1)]
       (if (nil? op2)
         [(format "%s IS NULL" attr) []]
         [(format "%s = ?" attr) [op2]]))))

(defn not-equal
  ([value]
     (operator-fn "<> %s" [value]))
  ([op1 op2]
     (let [attr (parse-symbol op1)]
       (if (nil? op2)
         [(format "%s IS NOT NULL" attr) []]
         [(format "%s <> ?" attr) [op2]]))))

(defmacro not-nil
  [attr op1 op2 & body]
  `(let [~attr (parse-symbol ~op1)]
     (if (nil? ~op2)
       (throw (IllegalArgumentException. "op2 must not be nil"))
       (do
         ~@body))))

(defn greater-than
  ([value]
     (operator-fn "> %s" [value]))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s > ?" attr) [op2]])))

(defn greater-than-or-equal
  ([value]
     (operator-fn ">= %s" [value]))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s >= ?" attr) [op2]])))

(defn less-than
  ([value]
     (operator-fn "< %s" [value]))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s < ?" attr) [op2]])))

(defn less-than-or-equal
  ([value]
     (operator-fn "<= %s" [value]))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s <= ?" attr) [op2]])))

(defn like
  ([pattern]
     (operator-fn "LIKE %s" [pattern]))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s LIKE ?" attr) [op2]])))

(defn not-like
  ([pattern]
     (operator-fn "NOT LIKE %s" [pattern]))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s NOT LIKE ?" attr) [op2]])))

(defn between
  ([value1]
     (operator-fn "BETWEEN %s" value1 " AND "))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s BETWEEN ? AND ?" attr) op2])))

(defn not-between
  ([value1]
     (operator-fn "NOT BETWEEN %s" value1 " AND "))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format "%s NOT BETWEEN ? AND ?" attr) op2])))

(defn in
  ([values]
     (operator-fn "IN (%s)" values ", "))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format
         (str "%s IN ("
              (str-utils/str-join ", " (map (fn [_] "?") op2))
              ")") attr) (apply vector op2)])))

(defn not-in
  ([values]
     (operator-fn "NOT IN (%s)" values ", "))
  ([op1 op2]
     (not-nil attr op1 op2
       [(format
         (str "%s NOT IN ("
              (str-utils/str-join ", " (map (fn [_] "?") op2))
              ")") attr) (apply vector op2)])))

(defmethod parse-condition clojure.lang.IPersistentMap
  [condition]
  ;; (prn condition)
  (if (> (count condition) 1)
    (apply and-conditions (map #(hash-map (first %) (second %)) condition))
    (let [k (first (keys condition))
          v (first (vals condition))]
      (if (fn? v)
        (v k)
        (equal k v)))))

(defmethod parse-condition clojure.lang.IPersistentVector
  [condition]
  (if (vector? (second condition))
    condition
    [(first condition) (into [] (rest condition))]))

(defmethod parse-condition :default
  [condition]
  condition)

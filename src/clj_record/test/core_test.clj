(ns clj-record.test.core-test
  (:require
    [clj-record.core :as core]
    [clj-record.query :as query]
    [clj-record.test.model.manufacturer :as manufacturer]
    [clj-record.test.model.product :as product])
  (:use clojure.contrib.test-is
        clj-record.test.test-helper))


(deftest table-name-can-be-unconventional-with-table-name-option-to-init-model
  (is (= "productos" (core/table-name "product"))))

(deftest table-name-is-available-on-each-model-namespace
  (are (= _1 _2)
    "manufacturers"  (manufacturer/table-name)
    "productos"       (product/table-name)))

(defdbtest insert-returns-id-of-new-record
  (let [id (manufacturer/insert (valid-manufacturer-with {:name "ACME"}))
        acme (manufacturer/get-record id)]
    (is (= "ACME" (acme :name)))))

(defdbtest get-record-by-id
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))]
    (is (= humedai (manufacturer/get-record (humedai :id))))))

(defdbtest find-records-by-attribute-equality-conditions
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))
        other-1 (manufacturer/create (valid-manufacturer-with {:name "Some Other"}))]
    (is (= [humedai] (manufacturer/find-records {:name "Humedai Motors"})))))

(defdbtest find-records-by-SQL-conditions
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))
        other-1 (manufacturer/create (valid-manufacturer-with {:name "Some Other"}))]
    (is (= [humedai] (manufacturer/find-records ["name = ?" "Humedai Motors"])))))

(defdbtest get-record-includes-metadata
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))]
    (is (= :manufacturer (:model ^(manufacturer/get-record (humedai :id)))))))

(defdbtest find-records-include-metadata
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))
        other-1 (manufacturer/create (valid-manufacturer-with {:name "Some Other"}))]
    (is (= :manufacturer (:model ^(first (manufacturer/find-records ["name = ?" "Humedai Motors"])))))))

(defdbtest find-by-sql-uses-a-complete-query
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))]
    (is (= (manufacturer/find-records ["name = ?" "Humedai Motors"])
           (manufacturer/find-by-sql ["SELECT * FROM manufacturers WHERE name = ?" "Humedai Motors"])))))

(defdbtest find-records-with-no-params-returns-all-rows
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (is (= [sozooke foyoto gmb ghysler merledas] (manufacturer/find-records)))))

(defdbtest find-records-can-be-ordered
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers2)]
    ;; NB: Capital letters sort before lowercase.
    (is (= [foyoto gmb ghysler merledas sozooke]
           (manufacturer/find-records :order :name)))))

(defdbtest find-records-with-attribute-equality-can-be-ordered
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers2)]
    (is (= [merledas gmb]
           (manufacturer/find-records {:grade 89} :order :founded)))))

(defdbtest find-records-can-order-with-direction
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers2)]
    (is (= [merledas gmb]
           (manufacturer/find-records {:grade 89} :order {:founded :asc})))
    (is (= [gmb merledas]
           (manufacturer/find-records {:grade 89} :order {:founded :desc}))))
)

(defdbtest find-records-can-be-ordered-by-multiple-conditions
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers2)]
    (is (= [merledas ghysler foyoto gmb sozooke]
           (manufacturer/find-records :order [:founded :grade])))
    (is (= [gmb sozooke ghysler foyoto merledas]
           (manufacturer/find-records :order [{:founded :desc} :grade])))
    (is (= [sozooke gmb foyoto ghysler merledas]
           (manufacturer/find-records :order [{:founded :desc} {:grade :desc}])))))

(comment
  ;; Limiting does not work with Derby
 (defdbtest find-records-can-be-limited
   (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
     (is (= [sozooke foyoto] (manufacturer/find-records :limit 2))))))

(defdbtest destroy-record-destroys-by-id-from-record
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors"}))]
    (manufacturer/destroy-record {:id (humedai :id)})
    (is (empty? (manufacturer/find-records {:id (humedai :id)})))))

(defdbtest update-uses-id-to-update-other-given-attributes-leaving-unspecified-attributes-untouched
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Motors" :grade 90}))
        id (humedai :id)]
    (manufacturer/update {:id id :name "Schmoomdai Motors" :founded "2008"})
    (is (= 
      {:name "Schmoomdai Motors" :grade 90 :founded "2008"}
      (select-keys (manufacturer/get-record id) [:name :grade :founded])))))

(comment (deftest to-conditions
   (are (= _1 (core/to-conditions _2))
        ["a = ?" 1] {:a 1}
        ["a = ?" "one"] {:a "one"}
        ["a IS NULL"] {:a nil}
        ["a = ?" 2] {:a (query/equal 2)}
        ["a <> ?" "two"] {:a (query/not-equal "two")}
        ["a > ?" 3] {:a (query/greater-than 3)}
        ["a >= ?" 3] {:a (query/greater-than-or-equal 3)}
        ["a < ?" 3] {:a (query/less-than 3)}
        ["a <= ?" 3] {:a (query/less-than-or-equal 3)}
        ["a LIKE ?" "a%"] {:a (query/like "a%")}
        ["a NOT LIKE ?" "%s"] {:a (query/not-like "%s")}
        ["a BETWEEN ? AND ?" 1 5] {:a (query/between 1 5)}
        ["a NOT BETWEEN ? AND ?" 6 10] {:a (query/not-between 6 10)}
        ["a IN (?, ?, ?)" "foo" "bar" "baz"] {:a (query/in "foo" "bar" "baz")}
        ["a NOT IN (?, ?, ?)" 1 2 3] {:a (query/not-in 1 2 3)})
   (let [r (core/to-conditions {:a 1 :b 2})]
     (is (or (= r ["a = ? AND b = ?" 1 2]) (= r ["b = ? AND a = ?" 2 1]))))))

(deftest order-conditions
  (are (= _1 (core/order-conditions _2))
       "a" :a
       "a desc" {:a :desc}
       "a asc" {:a :asc}
       "a, b" [:a :b]
       "a asc, b" [{:a :asc} :b]
       "a asc, b desc" [{:a :asc} {:b :desc}]))

(deftest model-metadata-with-no-args
  (is (= 
    (@clj-record.meta/all-models-metadata "manufacturer")
    (manufacturer/model-metadata)))
  (is (contains? @(manufacturer/model-metadata) :validations)))

(deftest model-metadata-with-one-arg
  (is (map? (manufacturer/model-metadata :callbacks))))

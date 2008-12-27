(def db {:classname "org.apache.derby.jdbc.EmbeddedDriver"
         :subprotocol "derby"
         :subname "/tmp/clj_record.test.db"
         :create true})

(ns clj_record.test.main
  (:require [clojure.contrib.sql :as sql]
            [clojure.contrib.test-is :as test-is]
            [clj_record.test.model.manufacturer :as manufacturer]
            [clj_record.test.model.product :as product]))


(defn drop-tables []
  (try
    (sql/drop-table :manufacturer)
    (sql/drop-table :product)
    (catch Exception e)))

(defn create-tables []
  (sql/create-table :manufacturer
    [:id "INT" "GENERATED ALWAYS AS IDENTITY CONSTRAINT manufacturer_pk PRIMARY KEY"]
    [:name "VARCHAR(32)" "NOT NULL"]
    [:founded "VARCHAR(4)"]
    [:grade :real])
  (sql/create-table :product
    [:id "INT" "GENERATED ALWAYS AS IDENTITY CONSTRAINT product_pk PRIMARY KEY"]
    [:name "VARCHAR(32)" "NOT NULL"]
    [:price "INT"]
    [:manufacturer_id "INT" "NOT NULL"]))

(sql/with-connection db
  (sql/transaction
    (drop-tables)
    (create-tables)))

(println "Setup complete.")

(load "associations-test")
(load "core-test")
(test-is/run-all-tests)

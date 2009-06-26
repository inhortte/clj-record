(ns clj-record.test.associations-test
  (:require
    [clj-record.test.model.manufacturer :as manufacturer]
    [clj-record.test.model.product :as product]
    [clj-record.test.model.customer :as customer]
    [clj-record.test.model.order :as order])
  (:use clojure.contrib.test-is
        clj-record.test.test-helper))


(defdbtest belongs-to-creates-find-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})]
    (is (= humedai (product/get-manufacturer s3000xi)))))

(defdbtest belongs-to-with-as-creates-a-get-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})
        john-doe (customer/create {:name "John Doe"})
        order-1 (order/create {:customer_id (:id john-doe) :product (:id s3000xi) :date "2009-06-17 09:55:36.0"})]
    (is (= john-doe (order/get-purchaser order-1)))))

(defdbtest belongs-to-with-on-creates-a-get-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})
        john-doe (customer/create {:name "John Doe"})
        order-1 (order/create {:customer_id (:id john-doe) :product (:id s3000xi) :date "2009-06-17 09:55:36.0"})]
    (is (= s3000xi (order/get-product order-1)))))

(defdbtest has-many-creates-a-find-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})
        s3000xl (product/create {:name "S-3000xl" :manufacturer_id (:id humedai)})]
    (is (= [s3000xi s3000xl] (manufacturer/find-products humedai)))))

(defdbtest has-many-with-as-creates-a-find-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})
        john-doe (customer/create {:name "John Doe"})
        order-1 (order/create {:customer_id (:id john-doe) :product (:id s3000xi) :date "2009-06-17 09:55:36.0"})
        order-2 (order/create {:customer_id (:id john-doe) :product (:id s3000xi) :date "2009-06-17 11:10:13.0"})]
    (is (= [order-1 order-2] (customer/find-purchases john-doe)))))

(defdbtest has-many-with-on-creates-a-find-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})
        john-doe (customer/create {:name "John Doe"})
        order-1 (order/create {:customer_id (:id john-doe) :product (:id s3000xi) :date "2009-06-17 09:55:36.0"})
        order-2 (order/create {:customer_id (:id john-doe) :product (:id s3000xi) :date "2009-06-17 11:10:13.0"})]
    (is (= [order-1 order-2] (product/find-orders s3000xi)))))

(defdbtest has-many-creates-a-destroy-function
  (let [humedai (manufacturer/create (valid-manufacturer-with {:name "Humedai Automotive"}))
        s3000xi (product/create {:name "S-3000xi" :manufacturer_id (:id humedai)})
        s3000xl (product/create {:name "S-3000xl" :manufacturer_id (:id humedai)})]
    (manufacturer/destroy-products humedai)
    (is (empty? (manufacturer/find-products humedai)))))

(comment
(defdbtest find-records-can-do-eager-fetching-of-has-many-association
  (let [manu1 (manufacturer/create (valid-manufacturer-with {:name "manu1" :grade 99}))
        prod1 (product/create {:name "prod1" :manufacturer_id (:id manu1)})
        prod2 (product/create {:name "prod2" :manufacturer_id (:id manu1)})
        manu2 (manufacturer/create (valid-manufacturer-with {:name "manu2" :grade 99}))
        prod3 (product/create {:name "prod3" :manufacturer_id (:id manu2)})
        prod4 (product/create {:name "prod4" :manufacturer_id (:id manu2)})]
    (let [[eager-manu1 eager-manu2] (manufacturer/find-records {:grade 99} {:include [:products]})]
      (are (= _1 _2)
        "manu1" (:name eager-manu1)
        "manu2" (:name eager-manu2)
        [prod1 prod2] (:products eager-manu1)
        [prod3 prod4] (:products eager-manu2)))))
)
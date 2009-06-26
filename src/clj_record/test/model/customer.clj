(ns clj-record.test.model.customer
  (:require clj-record.boot)
  (:use clj-record.test.model.config))


(clj-record.core/init-model
  (:associations
    (has-many orders :as purchases)))

(ns clj-record.test.model.order
  (:require clj-record.boot)
  (:use clj-record.test.model.config))


(clj-record.core/init-model
  (:associations
    (belongs-to products :on product)
    (belongs-to customers :as purchaser)))

(ns clj-record.test.test-helper
  (:require [clj-record.core :as core]
            [clj-record.test.model.manufacturer :as manufacturer])
  (:use clojure.contrib.test-is))


(defmacro defdbtest [name & body]
  `(deftest ~name
    (rolling-back ~@body)))

(defmacro rolling-back [& body]
  `(core/transaction clj-record.test.model.config/db
    (try
      ~@body
      (finally
        (clojure.contrib.sql/set-rollback-only)))))

(defmacro restoring-ref [ref & body]
  `(let [old-value# (deref ~ref)]
    (try
      ~@body
      (finally
        (dosync (ref-set ~ref old-value#))))))

(def valid-manufacturer {:name "Valid Name" :founded "1999" :grade 99})

(defn valid-manufacturer-with [attributes] (merge valid-manufacturer attributes))

(defn manufacturers []
  [ (manufacturer/create (valid-manufacturer-with {:name "Sozooke Motors" :grade 90}))
    (manufacturer/create (valid-manufacturer-with {:name "Foyoto Auto" :grade 78}))
    (manufacturer/create (valid-manufacturer-with {:name "GMB Motors" :grade 89}))
    (manufacturer/create (valid-manufacturer-with {:name "Ghysler Inc" :grade 60}))
    (manufacturer/create (valid-manufacturer-with {:name "Merledas Automotive" :grade 39}))])

(defn manufacturers2 []
  (doall
   (map manufacturer/create
        [{:name "Sozooke Motors"      :founded 2000 :grade 90}
         {:name "Foyoto Auto"         :founded 1999 :grade 78}
         {:name "GMB Motors"          :founded 2000 :grade 89}
         {:name "Ghysler Inc"         :founded 1999 :grade 60}
         {:name "Merledas Automotive" :founded 1998 :grade 89}])))

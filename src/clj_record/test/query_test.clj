(ns clj-record.test.query-test
  (:require
    [clj-record.core :as core]
    [clj-record.query :as query]
    [clj-record.test.model.manufacturer :as manufacturer]
    [clj-record.test.model.product :as product])
  (:use clojure.contrib.test-is
        clj-record.test.test-helper))

(defdbtest find-records-using-equal
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{sozooke}
            (set (manufacturer/find-records _1)))
         {:grade (query/equal 90)}
         (query/equal :grade 90))))

(defdbtest find-records-using-not-equal
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{sozooke foyoto ghysler merledas}
            (set (manufacturer/find-records _1)))
         {:name (query/not-equal "GMB Motors")}
         (query/not-equal :name "GMB Motors"))))

(defdbtest find-records-using-greater-than
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{sozooke gmb}
            (set (manufacturer/find-records _1)))
         {:grade (query/greater-than 80)}
         (query/greater-than :grade 80))))

(defdbtest find-records-using-greater-than-or-equal
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{sozooke foyoto gmb ghysler}
            (set (manufacturer/find-records _1)))
         {:grade (query/greater-than-or-equal 60)}
         (query/greater-than-or-equal :grade 60))))

(defdbtest find-records-using-less-than
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{foyoto ghysler merledas}
            (set (manufacturer/find-records _1)))
         {:grade (query/less-than 80)}
         (query/less-than :grade 80))))

(defdbtest find-records-using-less-than-or-equal
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{ghysler merledas}
            (set (manufacturer/find-records _1)))
         {:grade (query/less-than-or-equal 60)}
         (query/less-than-or-equal :grade 60))))

(defdbtest find-records-using-like
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{gmb ghysler}
            (set (manufacturer/find-records _1)))
         {:name (query/like "G%")}
         (query/like :name "G%"))))

(defdbtest find-records-using-not-like
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{foyoto ghysler merledas}
            (set (manufacturer/find-records _1)))
         {:name (query/not-like "%Motors")}
         (query/not-like :name "%Motors"))))

(defdbtest find-records-using-between
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{foyoto ghysler}
            (set (manufacturer/find-records _1)))
         {:grade (query/between [40 80])}
         (query/between :grade [40 80]))))

(defdbtest find-records-using-not-between
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{sozooke gmb merledas}
            (set (manufacturer/find-records _1)))
         {:grade (query/not-between [40 80])}
         (query/not-between :grade [40 80]))))

(defdbtest find-records-using-in
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{gmb merledas}
            (set (manufacturer/find-records _1)))
         {:name (query/in ["GMB Motors" "Merledas Automotive"])}
         (query/in :name ["GMB Motors" "Merledas Automotive"]))))

(defdbtest find-records-using-not-in
  (let [[sozooke foyoto gmb ghysler merledas] (manufacturers)]
    (are (= #{sozooke foyoto ghysler}
            (set (manufacturer/find-records _1)))
         {:name (query/not-in ["GMB Motors" "Merledas Automotive"])}
         (query/not-in :name ["GMB Motors" "Merledas Automotive"]))))

(deftest attempt-to-supply-nil-value
  (are (thrown? IllegalArgumentException _1)
    (query/equal nil)
    (query/not-equal nil)
    (query/greater-than nil)
    (query/between [3 nil])
    (query/between [nil 3])))

(deftest multi-param-query-functions
  (is (query/equal :a 1) ["a = ?" [1]])
  (is (query/equal :a nil) ["a IS NULL" []])
  (is (query/not-equal :a 1) ["a <> ?" [1]])
  (is (query/not-equal :a nil) ["a IS NOT NULL" []])
  (is (query/greater-than :a 1) ["a > ?" [1]])
  (is (query/greater-than-or-equal :a 1) ["a >= ?" [1]])
  (is (query/less-than :a 1) ["a < ?" [1]])
  (is (query/less-than-or-equal :a 1) ["a <= ?" [1]])
  (is (query/like :a "G%") ["a LIKE ?" ["G%"]]))

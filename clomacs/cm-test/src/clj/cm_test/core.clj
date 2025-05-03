(ns cm-test.core
  (:require [libpython-clj2.python
             :refer [as-python as-jvm
                     ->python ->jvm
                     get-attr call-attr call-attr-kw
                     get-item initialize!
                     py. py.- py.. py*]
             :as py]
            [clojure.string :as str]
            [cheshire.core :as json]))


(initialize! :library-path "/usr/lib/libpython3.13.so")

(py/import-as kerykeion k)

(defn generate-report
  [{:keys [name city nation]
    [month day year hour minute] :birth}]
  (json/parse-string (py. (call-attr-kw k "AstrologicalSubject" ""
                                        {:name name
                                         :year year
                                         :month month
                                         :day day
                                         :hour hour
                                         :minute minute
                                         :city city
                                         :nation nation
                                         :houses_system_identifier "W"})
                          "json")
                     true))

(def kesh (generate-report {:name "Kesh"
                            :birth [9 25 2001 12 55]
                            :city "North Carolina"
                            :nation "USA"}))

(defn output-kesh []
  kesh)

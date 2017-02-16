(ns suina.xpath.qt3-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [clojure.xml :as xml]
            [suina.core :refer :all]))

#_(def qt3base "https://dev.w3.org/2011/QT3-test-suite/catalog.xml")

(def xmlzipper (zip/xml-zip (xml/parse "/home/vojtech/Dev/QT3TS/catalog.xml")))

(defn parse [file]
  (-> file xml/parse zip/xml-zip))

(defn run-ts [catalog]
  (let [zipper (parse catalog)]))

(deftest a-test
  
  (testing "Don't FIXME, I succeed."
    (is (= 1 1))))

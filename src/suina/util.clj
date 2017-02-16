(ns suina.util
  (:gen-class)
  (:import [java.net URI]))

(defn resolve-uri
  [parent relative]
  (cond
   (empty? parent) relative
   (empty? relative) parent
   :else (let [parent-uri (URI. parent)
               relative-uri (URI. relative)
               resolved-uri (.resolve parent-uri relative-uri)]
           (.toString resolved-uri))))


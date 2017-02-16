(ns suina.xml
  (:gen-class)
  (:import [javax.xml XMLConstants]))

(defrecord QName [uri local prefix])

;;; (We don't use javax.xml.namespace.QName as its str representations
;;; are not supported by the reader.)
(defn qn
  ([local] (qn nil local))
  ([uri local] (qn uri local nil))
  ([uri local prefix]
   (let [use-uri (if (empty? uri) nil uri)
         use-prefix (if (empty? prefix) nil prefix)]
     (QName. uri local prefix))))

(defn local-name
  [qname]
  (:local qname))

(defn ns-uri
  [qname]
  (:uri qname))

(defn prefix
  [qname]
  (:prefix qname))

;;; some useful constants
(def ^:const ns-xml XMLConstants/XML_NS_URI)
(def ^:const qn-xml-base (qn ns-xml "base"))
(def ^:const qn-xml-id (qn ns-xml "id"))
(def ^:const qn-xml-lang (qn ns-xml "lang"))

(def ^:const ns-xmlns XMLConstants/XMLNS_ATTRIBUTE_NS_URI)
(def ^:const qn-xmlns (qn ns-xmlns XMLConstants/XMLNS_ATTRIBUTE))

;;; 

(defn ns-decl?
  [qname]
  (= ns-xmlns (ns-uri qname)))

;;; TODO throw an error if not an xmlns qname?
(defn ns-prefix
  [qname]
  (when (and (ns-decl? qname)
             (not= qn-xmlns qname))
    (local-name qname)))


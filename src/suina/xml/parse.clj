(ns suina.xml.parse
  (:gen-class)
  (:require [suina.xml :refer :all]
            [suina.util :as util])
  (:import (javax.xml.stream XMLInputFactory
                             XMLStreamReader
                             XMLStreamConstants)))

(defn- location
  [^XMLStreamReader sreader]
  (let [location (.getLocation sreader)
        offset (.getCharacterOffset location)
        column (.getColumnNumber location)
        line (.getLineNumber location)
        resource (.getSystemId location)]  ;TODO
    {:offset offset :column column :line line :resource resource}))

(defn- evt
  [^XMLStreamReader sreader ctx m]
  (let [loc (location sreader)
        base-uri (-> ctx :base-uri first)
        lang (-> ctx :lang first)
        common {:location loc :base-uri base-uri :lang lang}
        e (merge common m)]
    {:evt e :ctx ctx}))

;;; 

(defn- start-document
  [^XMLStreamReader sreader ctx]
  (evt sreader ctx {:type :start-document}))

(defn- end-document
  [^XMLStreamReader sreader ctx]
  (evt sreader ctx {:type :end-document}))

(defn- attr-hash
  [^XMLStreamReader sreader]
  (let [regular (into {} (for [i (range (.getAttributeCount sreader))
                               :let [local-name (.getAttributeLocalName sreader i)
                                     ns-uri (.getAttributeNamespace sreader i)
                                     prefix (.getAttributePrefix sreader i)
                                     qname (qn local-name ns-uri prefix)
                                     value (.getAttributeValue sreader i)]]
                           [qname value]))
        with-ns-decls (into regular (for [i (range (.getNamespaceCount sreader))
                                          :let [ns-uri (.getNamespaceURI sreader i)
                                                prefix (.getNamespacePrefix sreader i)
                                                qname (if (empty? prefix)
                                                        qn-xmlns
                                                        (qn prefix ns-xmlns))]]
                                      [qname ns-uri]))]
    with-ns-decls))

(defn- start-element
  [^XMLStreamReader sreader ctx]
  (let [local-name (.getLocalName sreader)
        ns-uri (.getNamespaceURI sreader)
        prefix (.getPrefix sreader)
        qname (qn local-name ns-uri prefix)
        attrs (attr-hash sreader)
        ctx-base-uri (-> ctx :base-uri first)
        ctx-lang (-> ctx :lang first)
        base-uri (util/resolve-uri ctx-base-uri (get attrs qn-xml-base))
        lang (or (get attrs qn-xml-lang) ctx-lang)
        newctx (-> ctx
                   (update-in [:base-uri] #(cons base-uri %))
                   (update-in [:lang] #(cons lang %)))]
    (evt sreader newctx {:type :start-element :qname qname :attrs attrs})))

(defn- end-element
  [^XMLStreamReader sreader ctx]
  (let [local-name (.getLocalName sreader)
        ns-uri (.getNamespaceURI sreader)
        prefix (.getPrefix sreader)
        qname (qn local-name ns-uri prefix)
        newctx (-> ctx
                   (update-in [:base-uri] rest)
                   (update-in [:lang] rest))]
    (evt sreader ctx {:type :end-element :qname qname})))

(defn- text
  [^XMLStreamReader sreader ctx]
  (let [data (.getText sreader)]
    (evt sreader ctx {:type :text :data data})))

(defn- comm
  [^XMLStreamReader sreader ctx]
  (let [data (.getText sreader)]
    (evt sreader ctx {:type :comment :data data})))

(defn- cdata
  [^XMLStreamReader sreader ctx]
  (let [data (.getText sreader)]
    (evt sreader ctx {:type :cdata :data data})))

(defn- pi
  [^XMLStreamReader sreader ctx]
  (let [target (.getPITarget sreader)
        data (.getPIData sreader)]
    (evt sreader ctx {:type :pi :target target :data data})))

;; Note, sreader is mutable and mutated here in pull-seq, but it's
;; protected by a lazy-seq so it's thread-safe.
(defn- pull-seq-doc
  "Creates a seq of events. The XMLStreamConstants/SPACE clause below doesn't seem to
be triggered by the JDK StAX parser, but is by others. Leaving in to be more complete."
  [^XMLStreamReader sreader & [ctx]]
  (lazy-seq
   (loop []
     (condp == (.next sreader)
       XMLStreamConstants/START_ELEMENT
       (let [ec (start-element sreader ctx)
             evt (:evt ec)
             newctx (:ctx ec)]
         (cons evt
               (pull-seq-doc sreader newctx)))
       XMLStreamConstants/END_ELEMENT
       (let [ec (end-element sreader ctx)
             evt (:evt ec)
             newctx (:ctx ec)]
         (cons evt
               (pull-seq-doc sreader newctx)))
       XMLStreamConstants/CHARACTERS
       (let [ec (text sreader ctx)
             evt (:evt ec)
             newctx (:ctx ec)]
         (cons evt
               (pull-seq-doc sreader newctx)))
       XMLStreamConstants/COMMENT
       (let [ec (comm sreader ctx)
             evt (:evt ec)
             newctx (:ctx ec)]
         (cons evt
               (pull-seq-doc sreader newctx)))
       XMLStreamConstants/CDATA
       (let [ec (cdata sreader ctx)
             evt (:evt ec)
             newctx (:ctx ec)]
         (cons evt
               (pull-seq-doc sreader newctx)))
       XMLStreamConstants/PROCESSING_INSTRUCTION
       (let [ec (pi sreader ctx)
             evt (:evt ec)
             newctx (:ctx ec)]
         (cons evt
               (pull-seq-doc sreader newctx)))
       XMLStreamConstants/END_DOCUMENT
       (let [ec (end-document sreader ctx)
             evt (:evt ec)]
         (list evt))
         (recur)
       ))))

(defn- pull-seq
  "Creates a seq of events. The XMLStreamConstants/SPACE clause below doesn't seem to
be triggered by the JDK StAX parser, but is by others. Leaving in to be more complete."
  [^XMLStreamReader sreader & [ctx]]
  (let [ec (start-document sreader ctx)
        evt (:evt ec)
        newctx (:ctx ec)]
    (cons evt
          (pull-seq-doc sreader newctx))))

(def ^:private xml-input-factory-props
  {:allocator javax.xml.stream.XMLInputFactory/ALLOCATOR
   :coalescing javax.xml.stream.XMLInputFactory/IS_COALESCING
   :namespace-aware javax.xml.stream.XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references javax.xml.stream.XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities javax.xml.stream.XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating javax.xml.stream.XMLInputFactory/IS_VALIDATING
   :reporter javax.xml.stream.XMLInputFactory/REPORTER
   :resolver javax.xml.stream.XMLInputFactory/RESOLVER
   :support-dtd javax.xml.stream.XMLInputFactory/SUPPORT_DTD})

(defn new-xml-input-factory [props]
  (let [fac (javax.xml.stream.XMLInputFactory/newInstance)]
    (doseq [[k v] props
            :let [prop (xml-input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

;;; FIXME use XMLEventReader to get proper support for CDATA sections!!!
(defn- source-seq
  "Parses the XML InputSource source using a pull-parser. Returns
a lazy sequence of ParseEvent records. Accepts key pairs
with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
and xml-input-factory-props for more information."
  [s & {:as props}]
  (let [fac (new-xml-input-factory props)
        ;; Reflection on following line cannot be eliminated via a
        ;; type hint, because s is advertised by fn parse to be an
        ;; InputStream or Reader, and there are different
        ;; createXMLStreamReader signatures for each of those types.
        sreader (.createXMLStreamReader fac s)]
    (pull-seq sreader)))

(defn parse
  "Parses the source, which can be an
InputStream or Reader, and returns a lazy tree of Element records. Accepts key pairs
with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
and xml-input-factory-props for more information. Defaults coalescing true."
  [source & props]
  (apply source-seq source props))

(defn parse-str
  "Parses the passed in string to Clojure data structures. Accepts key pairs
with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
and xml-input-factory-props for more information. Defaults coalescing true."
  [s & props]
  (let [sr (java.io.StringReader. s)]
    (apply parse sr props)))



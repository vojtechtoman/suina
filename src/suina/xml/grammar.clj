(ns suina.xml.grammar
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.spec :as s]))


(defn get-nested
  [m k]
  (->> (tree-seq map? #(-> % vals flatten) m)
       (filter map?)
       (some k)))

(defn collect-nested
  [m k]
  (->> (tree-seq map? #(-> % vals flatten) m)
       (filter map?)
       (map k))) 

(defn genkw []
  (keyword (gensym)))

(defmacro literal [str]
  (let [parsed# (gensym "parsed")
        keywords# (repeatedly genkw)
        sets# (map (fn [c] #{c}) (seq str))]
    `(s/& (s/cat ~@(interleave keywords# sets#))
          (s/conformer
           (fn [~parsed#]
             (-> ~parsed# vals str/join))))))

(defmacro ws
  ([spec] `(ws ~spec :both))
  ([spec wspos]
   (let [parsed# (gensym "parsed")]
     `(s/& (s/cat ~@(if (#{:both :before} wspos)
                      [:before `(s/? ::S)])
                  :body ~spec
                  ~@(if (#{:both :after} wspos)
                      [:after `(s/? ::S)]))
           (s/conformer
            (fn [~parsed#]
              (:body ~parsed#)))))))

;; simple choices: X | Y | Z | ...
(defmacro simple-choice [& specs]
  (let [keywords (repeatedly genkw)
        pairs# (interleave keywords specs)
        parsed# (gensym)]
    `(s/& (s/alt ~@pairs#)
          (s/conformer
           (fn [~parsed#]
             (second ~parsed#))))))

;; simple 1-or-more SEP-separated lists: X (SEP X)*
(defmacro separated-list-of [spec separator]
  (let [s# (genkw)
        rest# (genkw)
        sep# (genkw)
        parsed# (gensym "parsed")]
    `(s/& (s/cat ~s# ~spec
                 ~rest# (s/* (s/cat ~sep# ~separator
                                    ~s# ~spec)))
          (s/conformer
           (fn [~parsed#]
             (collect-nested ~parsed# ~s#))))))

;; simple 1-or-more comma-separated lists: X ("," X)*
(defmacro comma-separated-list-of [spec]
  `(separated-list-of ~spec (ws #{\,})))

;;;
;;; Grammar rules from 'Extensible Markup Language (XML) 1.0' (https://www.w3.org/TR/REC-xml)
;;;

;;; [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
(defn xmlchar? [c & [exclusions]]
  (and (not (contains? exclusions c))
       (let [i (int c)]
         (or (#{0x9 0xA 0xD} i)
             (<= 0x20 i 0xD7FF)
             (<= 0xE000 i 0xFFFD)
             (<= 0x10000 i 0x10FFFF)))))
(s/def ::Char xmlchar?)

;;; [3] S ::= (#x20 | #x9 | #xD | #xA)+
(s/def ::S (s/+ #{\space \tab \return \newline}))

;;; [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] 
;;;                       | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F]  | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
;;;                       | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
(defn- namestartchar? [c]
  (let [i (int c)]
    (or (<= 0x61 i 0x7A)
        (<= 0x41 i 0x5A)
        (= \: c)
        (= \_ c)
        (<= 0xC0 i 0xD6)
        (<= 0xD8 i 0xF6)
        (<= 0xF8 i 0x2FF)
        (<= 0x370 i 0x37D)
        (<= 0x37F i 0x1FFF)
        (<= 0x200C i 0x200D)
        (<= 0x2070 i 0x218F)
        (<= 0x2C00 i 0x2FEF)
        (<= 0x3001 i 0xD7FF)
        (<= 0xF900 i 0xFDCF)
        (<= 0xFDF0 i 0xFFFD)
        (<= 0x10000 i 0xEFFFF) )))

(s/def ::NameStartChar namestartchar?)

;;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
(defn- namechar? [c]
  (or (namestartchar? c)
      (let [i (int c)]
        (or (= \- c)
            (= \. c)
            (<= 0x30 i 0x39)
            (= 0xB7 i)
            (<= 0x0300 i 0x036F)
            (<= 0x203F i 0x2040)))))

(s/def ::NameChar namechar?)

;;; [5] Name ::= NameStartChar (NameChar)*
(s/def ::Name (s/& (s/cat :first ::NameStartChar
                          :rest (s/* ::NameChar))
                   (s/conformer
                    (fn [parsed]
                      (concat [(:first parsed)] (:rest parsed))))))


;;;
;;; Grammar rules from 'Namespaces in XML 1.0' (https://www.w3.org/TR/REC-xml-names)
;;;

;;; [4] NCName ::= Name - (Char* ':' Char*)
(s/def ::NCName-nonWS (s/& ::Name
                             (s/conformer
                              (fn [parsed]
                                (if (some #{\:} parsed)
                                  :clojure.spec/invalid
                                  parsed)))))
(s/def ::NCName (ws ::NCName-nonWS))

;;; [11] LocalPart ::= NCName
(s/def ::LocalPart ::NCName)

;;; [10] Prefix ::= NCName
(s/def ::Prefix ::NCName)

;;; [9] UnprefixedName ::= LocalPart 
(s/def ::UnprefixedName ::LocalPart)

;;; [8] PrefixedName ::= Prefix ':' LocalPart 
(s/def ::PrefixedName (s/& (s/cat :prefix ::Prefix
                                  :colon #{\:}
                                  :local ::LocalPart)
                           (s/conformer
                            (fn [parsed]
                              (select-keys parsed [:prefix :local])))))

;;; [7] QName ::= PrefixedName | UnprefixedName
(s/def ::QName (ws (s/& (s/alt :prefixed ::PrefixedName
                               :unprefixed ::UnprefixedName)
                        (s/conformer
                         (fn [parsed]
                           (let [type (first parsed)
                                 qname (second parsed)]
                             (if (= :unprefixed type)
                               {:local qname}
                               qname)))))))


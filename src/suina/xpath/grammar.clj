(ns suina.xpath.grammar
  (:gen-class)
  (:require [suina.xml.grammar :refer :all :as xmlg]
            [clojure.spec :as s]))


(defn- get-nested
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (some k))) 

;;;
;;; Grammar rules from 'XML Path Language (XPath) 3.1' (https://www.w3.org/TR/2014/CR-xpath-31-20141218)
;;;


(defn- digit? [c]
  (<= 0x30 (int c) 0x39))

;;; [126] CommentContents ::= (Char+ - (Char* ('(:' | ':)') Char*))
(s/def ::CommentContents (s/& (s/+ ::xmlg/Char)
                              (s/conformer
                               (fn [parsed]
                                 (if (some #{[\( \:] [\: \)]} (partition 2 1 parsed))
                                   :clojure.spec/invalid
                                   parsed)))))

;;; [125] Digits ::= [0-9]+
(s/def ::Digits (s/+ digit?))

;;; [124] Char ::= [http://www.w3.org/TR/REC-xml#NT-Char]
;;; see [2] Char

;;; [123] NCName ::= [http://www.w3.org/TR/REC-xml-names/#NT-NCName]
;;; see [4] NCNane

;;; [122] QName ::= [http://www.w3.org/TR/REC-xml-names/#NT-QName]
;;; see [7] QName

;;; [121] Comment ::= "(:" (CommentContents | Comment)* ":)"
(s/def ::Comment (ws (s/& (s/cat :open (literal "(:")
                                 :contents ::CommentContents
                                 :close (literal ":)"))
                          (s/conformer
                           (fn [parsed]
                             (:contents parsed))))))

;;; [120] EscapeApos ::= "''"
(s/def ::EscapeApos (s/& (literal "''")
                         (s/conformer
                          (fn [_] \'))))

;;; [119] EscapeQuot ::= '""'
(s/def ::EscapeQuot (s/& (literal "\"\"")
                         (s/conformer
                          (fn [_] \"))))

;;; [118] BracedURILiteral ::= "Q" "{" [^{}]* "}"
(s/def ::BracedURILiteral (ws (s/& (s/cat :c1 (literal "Q{")
                                          :uri (s/* #(xmlchar? % #{\{ \}}))
                                          :c2 #{\}})
                                   (s/conformer
                                    (fn [parsed]
                                      (:uri parsed))))))

;;; [117] URIQualifiedName ::= BracedURILiteral NCName
(s/def ::URIQualifiedName (ws (s/& (s/cat :uri ::BracedURILiteral
                                          :local ::xmlg/NCName)
                                   (s/conformer
                                    (fn [parsed]
                                      parsed)))))

;;; [116] StringLiteral::= ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
(s/def ::StringLiteral (ws (s/& (s/alt :qs (s/cat :openquot #{\"}
                                                  :str (s/* (s/alt :escquot ::EscapeQuot
                                                                   :char #(xmlchar? % #{\"})))
                                                  :closequot #{\"})
                                       :as (s/cat :openapos #{\'}
                                                  :str (s/* (s/alt :escquot ::EscapeApos
                                                                   :char #(xmlchar? % #{\'})))
                                                  :closequot #{\'}))
                                (s/conformer
                                 (fn [parsed]
                                   (let [s (-> parsed second :str)]
                                     (map second s)))))))

;;; [115] DoubleLiteral ::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
(s/def ::DoubleLiteral (ws (s/& (s/cat :base (s/alt :b1 (s/cat :point #{\.}
                                                               :frac ::Digits)
                                                    :b2 (s/cat :int ::Digits
                                                               :pointfrac (s/? (s/cat :point #{\.}
                                                                                      :frac (s/* digit?)))))
                                       :e #{\e \E}
                                       :sign (s/? #{\+ \-})
                                       :exp ::Digits)
                                (s/conformer
                                 (fn [parsed]
                                   (let [exp (:exp parsed)
                                         sign (or (:sign parsed) \+)
                                         base-raw (-> parsed :base second)
                                         base (concat (:int base-raw) [\.] (get-nested base-raw :frac))]
                                     {:base base :exp exp :sign sign}))))))

;;; [114] DecimalLiteral ::= ("." Digits) | (Digits "." [0-9]*)
(s/def ::DecimalLiteral (ws (s/& (s/alt :d1 (s/cat :point #{\.}
                                                   :frac ::Digits)
                                        :d2 (s/cat :int ::Digits
                                                   :point #{\.}
                                                   :frac (s/* digit?)))
                                 (s/conformer
                                  (fn [parsed]
                                    (let [data (second parsed)
                                          int (get-nested data :int)
                                          frac (get-nested data :frac)]
                                      (concat int [\.] frac)))))))

;;; [113] IntegerLiteral ::= Digits
(s/def ::IntegerLiteral (ws ::Digits))

;;; [112] EQName ::= QName | URIQualifiedName
(s/def ::EQName (s/& (s/alt :qname ::xmlg/QName
                            :uqname ::URIQualifiedName)
                     (s/conformer
                      (fn [parsed]
                        (second parsed)))))

;;; [111] ParenthesizedItemType ::= "(" ItemType ")"
(s/def ::ParenthesizedItemType (s/& (s/cat :open (ws #{\(})
                                           :type ::ItemType
                                           :close (ws #{\)}))
                      (s/conformer
                       (fn [parsed]
                         (:type parsed)))))

;;; [110] TypedArrayTest ::= "array" "(" SequenceType ")"
(s/def ::TypedArrayTest (s/& (s/cat :c1 (ws (literal "array("))
                                    :type ::SequenceType
                                    :c2 (ws #{\)}))
                             (s/conformer
                              (fn [parsed]
                                (:type parsed)))))

;;; [109] AnyArrayTest ::= "array" "(" "*" ")"
(s/def ::AnyArrayTest (s/& (s/cat :c1 (ws (literal "array("))
                                  :c2 #{"*"}
                                  :c3 (ws (literal ")")))
                           (s/conformer
                            (fn [_]
                              :AnyArrayTest))))

;;; [8] ForExpr ::= SimpleForClause "return" ExprSingle
(s/def ::ForExpr (s/cat :sfc ::SimpleForClause
                        :return (ws (literal "return"))
                        :expr ::ExprSingle))

;;; [7] ExprSingle ::= ForExpr | LetExpr | QuantifiedExpr | IfExpr | OrExpr
(s/def ::ExprSingle (s/alt :forexpr ::ForExpr
                           :letexpr ::LetExpr
                           :qexpr ::QuantifiedExpr
                           :ifexpr ::IfExpr
                           :orexpr ::OrExpr))

;;; [6] Expr ::= ExprSingle ("," ExprSingle)*
(s/def ::Expr (s/cat :first ::ExprSingle
                     :rest (s/* (s/cat :comma (ws #{\'})
                                       :expr ::ExprSingle))))

;;; [1] XPath ::= Expr
(s/def ::XPath ::Expr)


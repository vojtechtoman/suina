(ns suina.xpath.grammar
  (:gen-class)
  (:require [clojure.spec :as s]
            [clojure.string :as str]))

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

(defmacro pp [spec fn]
  `(s/& ~spec
        (s/conformer ~fn)))

(defmacro todo [spec]
  "throws an exception as reminder of unimplemented (or partially implemented) rules"
  (let [p# (gensym)]
    `(pp ~spec
         (fn [~p#] (throw (UnsupportedOperationException. "TODO"))))))

(defmacro literal [str]
  (let [keywords# (repeatedly genkw)
        sets# (map (fn [c] #{c}) (seq str))]
    `(pp (s/cat ~@(interleave keywords# sets#))
         #(-> % vals str/join))))

(defmacro ws
  ([spec] `(ws ~spec :both))
  ([spec wspos]
   `(pp (s/cat ~@(if (#{:both :before} wspos)
                   [:before `(s/? ::S)])
               :body ~spec
               ~@(if (#{:both :after} wspos)
                   [:after `(s/? ::S)]))
        #(:body %))))

;; simple choices: X | Y | Z | ...
(defmacro simple-choice [& specs]
  (let [keywords (repeatedly genkw)
        pairs# (interleave keywords specs)]
    `(pp (s/alt ~@pairs#)
         #(second %))))

;; simple 1-or-more SEP-separated lists: X (SEP X)*
(defmacro separated-list-of [spec separator]
  (let [s# (genkw)
        rest# (genkw)
        sep# (genkw)]
    `(pp (s/cat ~s# ~spec
                ~rest# (s/* (s/cat ~sep# ~separator
                                   ~s# ~spec)))
         #(collect-nested % ~s#))))

;; simple 1-or-more comma-separated lists: X ("," X)*
(defmacro comma-separated-list-of [spec]
  `(separated-list-of ~spec (ws #{\,})))



(defmacro axis [name]
  (let [kw1# (genkw)
        kv2# (genkw)]
    `(s/cat ~kw1# (literal ~name)
            ~kv2# (literal "::"))))

;;;
;;; Grammar rules from 'Extensible Markup Language (XML) 1.0' (https://www.w3.org/TR/REC-xml)
;;;

;; [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
(defn xmlchar? [c & [exclusions]]
  (and (not (contains? exclusions c))
       (let [i (int c)]
         (or (#{0x9 0xA 0xD} i)
             (<= 0x20 i 0xD7FF)
             (<= 0xE000 i 0xFFFD)
             (<= 0x10000 i 0x10FFFF)))))
(s/def ::Char xmlchar?)

;; [3] S ::= (#x20 | #x9 | #xD | #xA)+
(s/def ::S (s/+ #{\space \tab \return \newline}))

;; [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] 
;;                       | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F]  | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
;;                       | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
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

;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
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

;; [5] Name ::= NameStartChar (NameChar)*
(s/def ::Name (pp (s/cat :first ::NameStartChar
                         :rest (s/* ::NameChar))
                  #(apply str (:first %) (:rest %))))

;;;
;;; Grammar rules from 'Namespaces in XML 1.0' (https://www.w3.org/TR/REC-xml-names)
;;;

;; [4] NCName ::= Name - (Char* ':' Char*)
(s/def ::NCName-nonWS (pp ::Name
                          #(if (str/index-of % \:)
                             :clojure.spec/invalid
                             %)))
(s/def ::NCName (ws ::NCName-nonWS))

;; [11] LocalPart ::= NCName
(s/def ::LocalPart ::NCName)

;; [10] Prefix ::= NCName
(s/def ::Prefix ::NCName)

;; [9] UnprefixedName ::= LocalPart 
(s/def ::UnprefixedName (pp ::LocalPart
                            #(assoc nil :local %)))

;; [8] PrefixedName ::= Prefix ':' LocalPart 
(s/def ::PrefixedName (pp (s/cat :prefix ::Prefix
                                 :colon #{\:}
                                 :local ::LocalPart)
                          #(select-keys % [:prefix :local])))

;; [7] QName ::= PrefixedName | UnprefixedName
(s/def ::QName (ws (simple-choice ::PrefixedName ::UnprefixedName)))

;;;
;;; Grammar rules from 'XML Path Language (XPath) 3.1' (https://www.w3.org/TR/2014/CR-xpath-31-20141218)
;;;

;; 'forward' declaration
(s/def ::ExprSingle nil)

(defn- digit? [c]
  (<= 0x30 (int c) 0x39))

;; [126] CommentContents ::= (Char+ - (Char* ('(:' | ':)') Char*))
(s/def ::CommentContents (pp (s/+ ::Char)
                             #(if (some #{[\( \:] [\: \)]} (partition 2 1 %))
                                :clojure.spec/invalid
                                (apply str %))))

;; [125] Digits ::= [0-9]+
(s/def ::Digits (s/+ digit?))

;; [124] Char ::= [http://www.w3.org/TR/REC-xml#NT-Char]
;; see [2] Char

;; [123] NCName ::= [http://www.w3.org/TR/REC-xml-names/#NT-NCName]
;; see [4] NCNane

;; [122] QName ::= [http://www.w3.org/TR/REC-xml-names/#NT-QName]
;; see [7] QName

;; [121] Comment ::= "(:" (CommentContents | Comment)* ":)"
(s/def ::Comment (ws (pp (s/cat :c1 (literal "(:")
                                :contents ::CommentContents
                                :c2 (literal ":)"))
                         #(:contents %))))

;; [120] EscapeApos ::= "''"
(s/def ::EscapeApos (pp (literal "''")
                        (fn [_] \')))

;; [119] EscapeQuot ::= '""'
(s/def ::EscapeQuot (pp (literal "\"\"")
                        (fn [_] \")))

;; [118] BracedURILiteral ::= "Q" "{" [^{}]* "}"
(s/def ::BracedURILiteral-nonWS (pp (s/cat :c1 (literal "Q{")
                                           :uri (s/* #(xmlchar? % #{\{ \}}))
                                           :c2 #{\}})
                                    #(->> % :uri (apply str))))

(s/def ::BracedURILiteral (ws ::BracedURILiteral-nonWS))

;; [117] URIQualifiedName ::= BracedURILiteral NCName
(s/def ::URIQualifiedName (ws (s/cat :uri ::BracedURILiteral-nonWS
                                     :local ::NCName-nonWS)))

;; [116] StringLiteral::= ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
(s/def ::StringLiteral (ws (pp (simple-choice (s/cat :openquot #{\"}
                                                     :str (s/* (simple-choice ::EscapeQuot #(xmlchar? % #{\"})))
                                                     :closequot #{\"})
                                              (s/cat :openapos #{\'}
                                                     :str (s/* (simple-choice ::EscapeApos #(xmlchar? % #{\'})))
                                                     :closequot #{\'}))
                               (fn [x] (->> x :str (apply str))))))

;; [115] DoubleLiteral ::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
(s/def ::DoubleLiteral (ws (pp (s/cat :base (simple-choice (s/cat :point #{\.}
                                                                  :frac ::Digits)
                                                           (s/cat :int ::Digits
                                                                  :pointfrac (s/? (s/cat :point #{\.}
                                                                                         :frac (s/* digit?)))))
                                      :e #{\e \E}
                                      :sign (s/? #{\+ \-})
                                      :exp ::Digits)
                               (fn [x]
                                 (let [iraw (get-nested x :int)
                                       i (if (nil? iraw) \0 (apply str iraw))
                                       fraw (get-nested x :frac)
                                       f (if (nil? fraw) \0 (apply str fraw))
                                       s (or (:sign x) \+)
                                       e (->> x :exp (apply str))
                                       all (str i \. f \E s e)]
                                   (read-string all))))))


;; [114] DecimalLiteral ::= ("." Digits) | (Digits "." [0-9]*)
(s/def ::DecimalLiteral (ws (pp (simple-choice (s/cat :point #{\.}
                                                      :frac ::Digits)
                                               (s/cat :int ::Digits
                                                      :point #{\.}
                                                      :frac (s/* digit?)))
                                                               (fn [x]
                                 (let [iraw (get-nested x :int)
                                       i (if (nil? iraw) \0 (apply str iraw))
                                       fraw (get-nested x :frac)
                                       f (if (nil? fraw) \0 (apply str fraw))
                                       all (str i \. f \M)]
                                   (read-string all))))))

;; [113] IntegerLiteral ::= Digits
(s/def ::IntegerLiteral (ws (pp ::Digits
                                #(->> % (apply str) read-string))))

;; [112] EQName ::= QName | URIQualifiedName
(s/def ::EQName (simple-choice ::QName ::URIQualifiedName))

;; [111] ParenthesizedItemType ::= "(" ItemType ")"
(s/def ::ParenthesizedItemType (todo (s/cat :c1 (ws #{\(})
                                            :type ::ItemType
                                            :c2 (ws #{\)}))))

;; [110] TypedArrayTest ::= "array" "(" SequenceType ")"
(s/def ::TypedArrayTest (todo (s/cat :c1 (ws (literal "array"))
                                     :c2 (ws #{\(})
                                     :seqtype ::SequenceType
                                     :c3 (ws #{\)}))))

;; [109] AnyArrayTest ::= "array" "(" "*" ")"
(s/def ::AnyArrayTest (todo (s/cat :c1 (ws (literal "array"))
                                   :c2 (ws #{\(})
                                   :c3 (ws #{\*})
                                   :c4 (ws #{\)}))))

;; [108] ArrayTest ::= AnyArrayTest | TypedArrayTest
(s/def ::ArrayTest (simple-choice ::AnyArrayTest ::TypedArrayTest))

;; [107] TypedMapTest ::= "map" "(" AtomicOrUnionType "," SequenceType ")"
(s/def ::TypedMapTest (todo (s/cat :c1 (ws (literal "map"))
                                   :c2 (ws #{\(})
                                   :autype ::AtomicOrUnionType
                                   :c3 (ws #{\,})
                                   :seqtype ::SequenceType
                                   :c2 (ws #{\)}))))

;; [106] AnyMapTest ::= "map" "(" "*" ")"
(s/def ::AnyMapTest (todo (s/cat :c1 (ws (literal "map"))
                                 :c2 (ws #{\(})
                                 :c3 (ws #{\*})
                                 :c4 (ws #{\)}))))

;; [105] MapTest ::= AnyMapTest | TypedMapTest
(s/def ::MapTest (simple-choice ::AnyMapTest ::TypedMapTest))

;; [104] TypedFunctionTest ::= "function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType
(s/def ::TypedFunctionTest (todo (s/cat :c1 (ws (literal "function"))
                                        :c2 (ws #{\(})
                                        :argtypes (s/? (comma-separated-list-of ::SequenceType))
                                        :c3 (ws (literal "as"))
                                        :returntype ::SequenceType)))

;; [103] AnyFunctionTest ::= "function" "(" "*" ")"
(s/def ::AnyFunctionTest (todo (s/cat :c1 (ws (literal "function"))
                                      :c2 (ws #{\(})
                                      :c3 (ws #{\*})
                                      :c4 (ws #{\)}))))

;; [102] FunctionTest ::= AnyFunctionTest | TypedFunctionTest
(s/def ::FunctionTest (simple-choice ::AnyFunctionTest ::TypedFunctionTest))

;; [101] TypeName ::= EQName
(s/def ::TypeName ::EQName)

;; [100] SimpleTypeName ::= TypeName
(s/def ::SimpleTypeName ::TypeName)

;; [99] ElementName ::= EQName
(s/def ::ElementName ::EQName)

;; [98] AttributeName ::= EQName
(s/def ::AttributeName ::EQName)

;; [97] ElementDeclaration ::= ElementName
(s/def ::ElementDeclaration ::ElementName)

;; [96] SchemaElementTest ::= "schema-element" "(" ElementDeclaration ")"
(s/def ::SchemaElementTest (s/cat :c1 (ws (literal "schema-element"))
                                  :c2 (ws #{\(})
                                  :decl ::ElementDeclaration
                                  :c3 (ws #{\)})))

;; [95] ElementNameOrWildcard ::= ElementName | "*"
(s/def ::ElementNameOrWildcard (simple-choice ::ElementName (ws #{\*})))

;; [94] ElementTest ::= "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
(s/def ::ElementTest (s/cat :c1 (ws (literal "element"))
                            :c2 (ws #{\(})
                            :args (s/? (s/cat :ew ::ElementNameOrWildcard
                                              :rest (s/? (s/cat :comma (ws #{\,})
                                                                :type ::TypeName
                                                                :q (s/? (ws #{\?}))))))
                            :c3 (ws #{\)})))

;; [93] AttributeDeclaration ::= AttributeName
(s/def ::AttributeDeclaration ::AttributeName)

;; [92] SchemaAttributeTest ::= "schema-attribute" "(" AttributeDeclaration ")"
(s/def ::SchemaAttributeTest (s/cat :c1 (ws (literal "schema-attribute"))
                                    :c2 (ws #{\(})
                                    :decl ::AttributeDeclaration
                                    :c3 (ws #{\)})))

;; [91] AttribNameOrWildcard ::= AttributeName | "*"
(s/def ::AttribNameOrWildcard (simple-choice ::AttributeName (ws #{\*})))

;; [90] AttributeTest ::= "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
(s/def ::AttributeTest (s/cat :c1 (ws (literal "attribute"))
                              :c2 (ws #{\(})
                              :args (s/? (s/cat :ew ::AttribNameOrWildcard
                                                :rest (s/? (s/cat :comma (ws #{\,})
                                                                  :type ::TypeName))))
                              :c3 (ws #{\)})))

;; [89] PITest ::= "processing-instruction" "(" (NCName | StringLiteral)? ")"
(s/def ::PITest (s/cat :c1 (ws (literal "processing-instruction"))
                       :c2 (ws #{\(})
                       :arg (s/? (s/alt :ncname ::NCName
                                        :stringliteral ::StringLiteral))
                       :c3 (ws #{\)})))

;; [88] NamespaceNodeTest ::= "namespace-node" "(" ")"
(s/def ::NamespaceNodeTest (todo (s/cat :c1 (ws (literal "namespace-node"))
                                        :c2 (ws #{\(})
                                        :c3 (ws #{\)}))))

;; [87] CommentTest ::= "comment" "(" ")"
(s/def ::CommentTest (todo (s/cat :c1 (ws (literal "comment"))
                                  :c2 (ws #{\(})
                                  :c3 (ws #{\)}))))

;; [86] TextTest ::= "text" "(" ")"
(s/def ::TextTest (todo (s/cat :c1 (ws (literal "text"))
                               :c2 (ws #{\(})
                               :c3 (ws #{\)}))))

;; [85] DocumentTest ::= "document-node" "(" (ElementTest | SchemaElementTest)? ")"
(s/def ::DocumentTest (todo (s/cat :c1 (ws (literal "document-node"))
                                   :c2 (ws #{\(})
                                   :arg (s/? (simple-choice ::ElementTest ::SchemaElementTest))
                                   :c3 (ws #{\)}))))

;; [84] AnyKindTest ::= "node" "(" ")"
(s/def ::AnyKindTest (todo (s/cat :c1 (ws (literal "node"))
                                  :c2 (ws #{\(})
                                  :c3 (ws #{\)}))))



;; [83] KindTest ::= DocumentTest | ElementTest | AttributeTest | SchemaElementTest
;;                    | SchemaAttributeTest | PITest | CommentTest | TextTest
;;                    | NamespaceNodeTest | AnyKindTest
(s/def ::KindTest (simple-choice ::DocumentTest ::ElementTest ::AttributeTest
                                 ::SchemaElementTest ::SchemaAttributeTest ::PITest
                                 ::CommentTest ::TextTest ::NamespaceNodeTest ::AnyKindTest))

;; [82] AtomicOrUnionType ::= EQName
(s/def ::AtomicOrUnionType ::EQName)

;; [81] ItemType ::= KindTest | ("item" "(" ")") | FunctionTest | MapTest | ArrayTest
;;                   | AtomicOrUnionType | ParenthesizedItemType
(s/def ::ItemTest (todo (s/cat :c1 (ws (literal "item"))  ; helper
                               :c2 (ws #{\(})
                               :c3 (ws #{\)}))))
(s/def ::ItemType (simple-choice ::KindTest ::ItemTest ::FunctionTest ::MapTest
                                 ::ArrayTest ::AtomicOrUnionType ::ParenthesizedItemType))

;; [80] OccurrenceIndicator ::= "?" | "*" | "+"
(s/def ::OccurrenceIndicator (simple-choice (ws #{\?})
                                            (ws #{\*})
                                            (ws #{\+})))

;; [79] SequenceType ::= ("empty-sequence" "(" ")") | (ItemType OccurrenceIndicator?)
(s/def ::EmptySequence (todo (s/cat :c1 (ws (literal "empty-sequence"))  ;;helper
                                    :c2 (ws #{\(})
                                    :c3 (ws #{\)}))))

(s/def ::SequenceType (simple-choice ::EmptySequence
                                     (s/cat :type ::ItemType
                                            :occur (s/? ::OccurrenceIndicator))))

;; [78] TypeDeclaration ::= "as" SequenceType
(s/def ::TypeDeclaration (s/cat :as (ws (literal "as"))
                                :type ::SequenceType))

;; [77] SingleType ::= SimpleTypeName "?"?
(s/def ::SingleType (s/cat :type ::SimpleTypeNam
                           :opt (s/? (ws #{\?}))))

;; [76] UnaryLookup ::= "?" KeySpecifier
(s/def ::UnaryLookup (s/cat :lookup (ws #{\?})
                            :ks ::KeySpecifier))

;; [75] CurlyArrayConstructor ::= "array" "{" Expr? "}"
(s/def ::CurlyArrayConstructor (s/cat :c1 (ws (literal "array"))
                                      :c2 (ws #{\{})
                                      :expr (s/? ::Expr)
                                      :c3 (ws #{\}})))

;; [74] SquareArrayConstructor ::= "[" (ExprSingle ("," ExprSingle)*)? "]"
(s/def ::SquareArrayConstructor (s/cat :c1 (ws #{\[})
                                       :ex1 (s/? (comma-separated-list-of ::ExprSingle))
                                       :c2 (ws #{\]})))


;; [73] ArrayConstructor ::= SquareArrayConstructor | CurlyArrayConstructor
(s/def ::ArrayConstructor (simple-choice ::SquareArrayConstructor ::CurlyArrayConstructor))

;; [72] MapValueExpr ::= ExprSingle
(s/def ::MapValueExpr ::ExprSingle)

;; [71] MapKeyExpr ::= ExprSingle
(s/def ::MapKeyExpr ::ExprSingle)

;; [70] MapConstructorEntry ::= MapKeyExpr ":" MapValueExpr
(s/def ::MapConstructorExpr (s/cat :key ::MapKeyExpr
                                   :comma (ws #{\:})
                                   :value ::MapValueExpr))

;; [69] MapConstructor ::= "map" "{" (MapConstructorEntry ("," MapConstructorEntry)*)? "}"
(s/def ::MapConstructor (s/cat :c1 (ws (literal "map"))
                               :c2 (ws #{\{})
                               :ex1 (s/? (comma-separated-list-of ::MapConstructorEntry))
                               :c3 (ws #{\}})))

;; [68] InlineFunctionExpr ::= "function" "(" ParamList? ")" ("as" SequenceType)? FunctionBody
(s/def ::InlineFunctionExpr (s/cat :c1 (ws (literal "function"))
                                   :c2 (ws #{\(})
                                   :params (s/? ::ParamList)
                                   :c3 (ws #{\)})
                                   :rtype (s/? (s/cat :as (ws (literal "as"))
                                                      :type ::SequenceType))
                                   :body ::FunctionBody))

;; [67] NamedFunctionRef ::= EQName "#" IntegerLiteral
(s/def ::NamedFunctionRef (s/cat :name ::EQname
                                 :pound (ws #{\#})
                                 :arity ::IntegerLiteral))

;; [66] FunctionItemExpr ::= NamedFunctionRef | InlineFunctionExpr
(s/def ::FunctionItemExpr (simple-choice ::NamedFunctionRef ::InlineFunctionExpr))

;; [65] ArgumentPlaceholder ::= "?"
(s/def ::ArgumentPlaceholder (ws #{\?}))

;; [64] Argument ::= ExprSingle | ArgumentPlaceholder
(s/def ::Argument (simple-choice ::ExprSingle ::ArgumentPlaceholder))

;; [63] FunctionCall ::= EQName ArgumentList
(s/def ::FunctionCall (s/cat :name ::EQName
                             :args ::ArgumentList))

;; [62] ContextItemExpr ::= "."
(s/def ::ContextItemExpr (ws #{\.}))

;; [61] ParenthesizedExpr ::= "(" Expr? ")"
(s/def ::ParenthesizedExpr (s/cat :c1 (ws #{\(})
                                  :expr (s/? ::Expr)
                                  :c2 (ws #{/})))

;; [60] VarName ::= EQName
(s/def ::VarName ::EQName)

;; [59] VarRef ::= "$" VarName
(s/def ::VarRef (s/cat :c1 (ws #{\$})
                       :var ::VarName))

;; [58] NumericLiteral ::= IntegerLiteral | DecimalLiteral | DoubleLiteral
(s/def ::NumericLiteral (simple-choice ::IntegerLiteral ::DecimalLiteral ::DoubleLiteral))

;; [57] Literal ::= NumericLiteral | StringLiteral
(s/def ::Literal (simple-choice ::NumericLiteral ::StringLiteral))

;; [56] PrimaryExpr ::= Literal | VarRef | ParenthesizedExpr | ContextItemExpr
;;                      | FunctionCall | FunctionItemExpr | MapConstructor
;;                      | ArrayConstructor | UnaryLookup
(s/def ::PrimaryExpr (simple-choice ::Literal ::VarRef ::ParenthesizedExpr
                                    ::ContextItemExpr ::FunctionCall ::FunctionItemExpr
                                    ::MapConstructor ::ArrayConstructor ::UnaryLookup))

;; [55] ArrowFunctionSpecifier ::= EQName | VarRef | ParenthesizedExpr
(s/def ::ArrowFunctionSpecifier (simple-choice ::EQName ::VarRef ::ParenthesizedExpr))

;; [54] ArrowPostfix ::= "=>" ArrowFunctionSpecifier ArgumentList
(s/def ::ArrowPostfix (s/cat :c1 (ws (literal "=>"))
                             :spec ::ArrowFunctionSpecifier
                             :args ::ArgumentList))

;; [53] KeySpecifier ::= NCName | IntegerLiteral | ParenthesizedExpr | "*"
(s/def ::KeySpecifier (simple-choice ::NCName ::IntegerLiteral ::ParenthesizedExpr (ws #{\*})))

;; [52] Lookup ::= "?" KeySpecifier
(s/def ::Lookup (s/cat :c1 (ws #{\?})
                       :spec ::KeySpecifier))

;; [51] Predicate ::= "[" Expr "]"
(s/def ::Predicate (s/cat :c1 (ws #{\[})
                          :expr ::Expr
                          :c1 (ws #{\]})))

;; [50] PredicateList ::= Predicate*
(s/def ::PredicateList (s/* ::Predicate))

;; [49] ArgumentList ::= "(" (Argument ("," Argument)*)? ")"
(s/def ::ArgumentList (s/cat :c1 (ws #{\(})
                             :args (comma-separated-list-of ::Argument)
                             :c2 (ws #{\)})))

;; [48] PostfixExpr ::= PrimaryExpr (Predicate | ArgumentList | Lookup | ArrowPostfix)*
(s/def ::PostfixExpr (s/cat :expr ::PrimaryExpr
                            :rest (simple-choice ::Predicate ::ArgumentList ::Lookup ::ArrowPostfix)))

;; [47] Wildcard ::= "*" | (NCName ":" "*") | ("*" ":" NCName) | (BracedURILiteral "*") /* ws: explicit */
(s/def ::Wildcard (pp (simple-choice (ws #{\*})
                                     (s/cat :prefix ::NCName
                                            :c1 #{\:}
                                            :local (ws #{\*} :after))
                                     (s/cat :prefix (ws #{\*} :before)
                                            :c2 #{\:}
                                            :local ::NCName)
                                     (s/cat :uri ::BracedURILiteral
                                            :local (ws #{\*} :after)))
                      #(if (map? %) (select-keys % [:uri :local :prefix]) %)))

;; [46] NameTest ::= EQName | Wildcard
(s/def ::NameTest (simple-choice ::EQName ::Wildcard))

;; [45] NodeTest ::= KindTest | NameTest
(s/def ::NodeTest (simple-choice ::KindTest ::NameTest))

;; [44] AbbrevReverseStep ::= ".."
(s/def ::AbbrevReverseStep (ws (literal "..")))

;; [43] ReverseAxis ::= ("parent" "::") | ("ancestor" "::") | ("preceding-sibling" "::") | ("preceding" "::") | ("ancestor-or-self" "::")
(s/def ::ReverseAxis (simple-choice (axis "parent") (axis "preceding-sibling")
                                    (axis "preceding") (axis "ancestor-or-self")))

;; [42] ReverseStep ::= (ReverseAxis NodeTest) | AbbrevReverseStep
(s/def ::ReverseStep (simple-choice (s/cat :axis ::ReverseAxis
                                           :nodetest ::NodeTest)
                                    ::AbbrevReverseStep))

;; [41] AbbrevForwardStep ::= "@"? NodeTest
(s/def ::AbbrevForwardStep (s/cat :attr? (ws #{\@})
                                  :nodetest ::NodeTest))

;; [40] ForwardAxis ::= ("child" "::") | ("descendant" "::") | ("attribute" "::") | ("self" "::") | ("descendant-or-self" "::") | ("following-sibling" "::") | ("following" "::") | ("namespace" "::")
(s/def ::ForwardAxis (simple-choice (axis "child") (axis "descendant") (axis "attribute")
                                    (axis "self") (axis "descendant-or-self") (axis "following-sibling")
                                    (axis "following") (axis "namespace")))

;; [39] ForwardStep ::= (ForwardAxis NodeTest) | AbbrevForwardStep
(s/def ::ForwardStep (simple-choice (s/cat :axis ::ForwardAxis
                                           :nodetest ::NodeTest)
                                    ::AbbrevForwardStep))

;; [38] AxisStep ::= (ReverseStep | ForwardStep) PredicateList
(s/def ::AxisStep (s/cat :step (simple-choice ::ReverseStep ::ForwardStep)
                         :predicates ::Predicatelist))

;; [37] StepExpr ::= PostfixExpr | AxisStep
(s/def ::StepExpr (simple-choice ::PostfixExpr ::AxisStep))

;; [36] RelativePathExpr ::= StepExpr (("/" | "//") StepExpr)*
;; FIXME
(s/def ::RelativePathExpr (separated-list-of ::StepExpr (simple-choice (ws #{\/}) (ws (literal "//")))))

;; [35] PathExpr ::= ("/" RelativePathExpr?) | ("//" RelativePathExpr) | RelativePathExpr /* xgc: leading-lone-slash */
;; FIXME
(s/def ::PathExpr (simple-choice (s/cat :c1 (ws #{\/})
                                        :pathexpr (s/? ::RelativePathExpr))
                                 (s/cat :c1 (ws (literal "//"))
                                        :pathexpr ::RelativePathExpr)
                                 :pathexpr ::RelativePathExpr))

;; [34] SimpleMapExpr ::= PathExpr ("!" PathExpr)*
(s/def ::SimpleMapExpr (separated-list-of ::PathExpr (ws #{\!})))

;; [33] NodeComp ::= "is" | "<<" | ">>"
(s/def ::NodeComp (simple-choice (ws (literal "is"))
                                 (ws (literal "<<"))
                                 (ws (literal ">>"))))

;; [32] ValueComp ::= "eq" | "ne" | "lt" | "le" | "gt" | "ge"
(s/def ::ValueComp (simple-choice (ws (literal "eq"))
                                  (ws (literal "ne"))
                                  (ws (literal "lt"))
                                  (ws (literal "le"))
                                  (ws (literal "gt"))
                                  (ws (literal "ge"))))

;; [31] GeneralComp ::= "=" | "!=" | "<" | "<=" | ">" | ">="
(s/def ::GeneralComp (simple-choice (ws #{\=})
                                    (ws (literal "!="))
                                    (ws #{\<})
                                    (ws (literal "<="))
                                    (ws #{\>})
                                    (ws (literal ">="))))

;; [30] ValueExpr ::= SimpleMapExpr
(s/def ::ValueExpr ::SimpleMapExpr)

;; [29] UnaryExpr ::= ("-" | "+")* ValueExpr
(s/def ::UnaryExpr (s/cat :signs (simple-choice (ws #{\-}) (ws #{\+}))
                          :expr ::ValueExpr))

;; [28] CastExpr ::= UnaryExpr ( "cast" "as" SingleType )?
(s/def ::CastExpr (s/cat :expr ::UnaryExpr
                         :cast (s/? (s/cat :c1 (ws (literal "cast"))
                                           :c2 (ws (literal "as"))
                                           :type ::SingleType))))

;; [27] CastableExpr ::= CastExpr ( "castable" "as" SingleType )?
(s/def ::CastableExpr (s/cat :expr ::CastExpr
                             :castable (s/? (s/cat :c1 (ws (literal "castable"))
                                                   :c2 (ws (literal "as"))
                                                   :type ::SingleType))))

;; [26] TreatExpr ::= CastableExpr ( "treat" "as" SequenceType )?
(s/def ::TreatExpr (s/cat :expr ::CastableExpr
                          :treat (s/? (s/cat :c1 (ws (literal "treat"))
                                             :c2 (ws (literal "as"))
                                             :type ::SequenceType))))

;; [25] InstanceofExpr ::= TreatExpr ( "instance" "of" SequenceType )?
(s/def ::InstanceofExpr (s/cat :expr ::TreatExpr
                               :instance (s/? (s/cat :c1 (ws (literal "instance"))
                                                     :c2 (ws (literal "of"))
                                                     :type ::SequenceType))))

;; [24] IntersectExceptExpr ::= InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
(s/def ::IntersectExceptExpr (separated-list-of ::InstanceofExpr (simple-choice (ws (literal "intersect"))
                                                                                (ws (literal "except")))))

;; [23] UnionExpr ::= IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
(s/def ::UnionExpr (separated-list-of ::IntersectExceptExpr (simple-choice (ws (literal "union"))
                                                                           (ws #{\|}))))

;; [22] MultiplicativeExpr ::= UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
(s/def ::MultiplicativeExpr (separated-list-of ::UnionExpr (simple-choice (ws #{\*})
                                                                          (ws (literal "div"))
                                                                          (ws (literal "idiv"))
                                                                          (ws (literal "mode")))))

;; [21] AdditiveExpr ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
(s/def ::AdditiveExpr (separated-list-of ::MultiplicativeExpr (simple-choice (ws #{\+})
                                                                             (ws #{\-}))))

;; [20] RangeExpr ::= AdditiveExpr ( "to" AdditiveExpr )?
(s/def ::RangeExpr (s/cat :expr ::AdditiveExpr
                          :to (s/? (s/cat :c1 (ws (literal "to"))
                                          :expr-to ::AdditiveExpr))))

;; [19] StringConcatExpr ::= RangeExpr ( "||" RangeExpr )*
(s/def ::StringConcatExpr (separated-list-of ::RangeExpr (ws (literal "||"))))

;; [18] ComparisonExpr ::= StringConcatExpr ( (ValueComp | GeneralComp | NodeComp) StringConcatExpr )?
(s/def ::ComparisonExpr (s/cat :expr ::StringConcatExpr
                               :comp (s/? (s/cat :comp (simple-choice ::ValueComp ::GeneralComp ::NodeComp)
                                                 :expr ::StringConcatExpr))))

;; [17] AndExpr ::= ComparisonExpr ( "and" ComparisonExpr )*
(s/def ::AndExpr (separated-list-of ::ComparisonExpr (ws (literal "and"))))

;; [16] OrExpr ::= AndExpr ( "or" AndExpr )*
(s/def ::OrExpr (separated-list-of ::AndExpr (ws (literal "or"))))

;; [15] IfExpr ::= "if" "(" Expr ")" "then" ExprSingle "else" ExprSingle
(s/def ::IfExpr (s/cat :c1 (ws (literal "if"))
                       :c2 (ws #{\(})
                       :expr ::Expr
                       :c3 (ws #{\)})
                       :c4 (ws (literal "then"))
                       :then ::ExprSingle
                       :c5 (ws (literal "else"))
                       :else ::ExprSingle))

;; [14] QuantifiedExpr ::= ("some" | "every") "$" VarName "in" ExprSingle
;;                         ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle
(s/def ::QuantifiedExpr (s/cat :some-every (simple-choice (ws (literal "some"))
                                                          (ws (literal "every")))
                               :c1 (ws #{\$})
                               :bindings (comma-separated-list-of (s/cat :var ::VarName
                                                                         :c1 (ws (literal "in"))
                                                                         :expr ::ExprSingle))
                               :c1 (ws (literal "satisfies"))
                               :expr ::ExprSingle))

;; [13] SimpleLetBinding ::= "$" VarName ":=" ExprSingle
(s/def ::SimpleForBinding (s/cat :c1 (ws #{\$})
                                 :var ::VarName
                                 :c2 (ws (literal ":="))
                                 :expr ::ExprSingle))

;; [12] SimpleLetClause ::= "let" SimpleLetBinding ("," SimpleLetBinding)*
(s/def ::SimpleLetClause (s/cat :c1 (ws (literal "let"))
                                :bindings (comma-separated-list-of ::SimpleLetBinding)))

;; [11] LetExpr ::= SimpleLetClause "return" ExprSingle
(s/def ::LetExpr (s/cat :let ::SimpleLetClause
                        :c1 (ws (literal "return"))
                        :expr ::ExprSingle))

;; [10] SimpleForBinding ::= "$" VarName "in" ExprSingle
(s/def ::SimpleForBinding (s/cat :c1 (ws #{\$})
                                 :var ::VarName
                                 :c2 (ws (literal "in"))
                                 :expr ::ExprSingle))

;; [9] SimpleForClause ::= "for" SimpleForBinding ("," SimpleForBinding)*
(s/def ::SimpleForClause (s/cat :let (ws (literal "for"))
                                :bindings (comma-separated-list-of ::SimpleForBinding)))

;; [8] ForExpr ::= SimpleForClause "return" ExprSingle
(s/def ::ForExpr (s/cat :sfc ::SimpleForClause
                        :return (ws (literal "return"))
                        :expr ::ExprSingle))

;; [7] ExprSingle ::= ForExpr | LetExpr | QuantifiedExpr | IfExpr | OrExpr
(s/def ::ExprSingle (simple-choice ::ForExpr ::LetExpr ::QuantifiedExpr ::IfExpr ::OrExpr))

;; [6] Expr ::= ExprSingle ("," ExprSingle)*
(s/def ::Expr (comma-separated-list-of ::ExprSingle))

;; [5] EnclosedExpr ::= "{" Expr "}"
(s/def ::EnclosedExpr (s/cat :c1 (ws #{\{})
                             :expr ::Expr
                             :c2 (ws #{\}})))

;; [4] FunctionBody ::= EnclosedExpr
(s/def ::FunctionBody ::EnclosedExpr)

;; [3] Param ::= "$" EQName TypeDeclaration?
(s/def ::Param (s/cat :c1 (ws #{\$})
                      :name ::EQName
                      :type (s/? ::TypeDeclaration)))

;; [2] ParamList ::= Param ("," Param)*
(s/def ::ParamList (comma-separated-list-of ::Param))

;; [1] XPath ::= Expr
(s/def ::XPath ::Expr)


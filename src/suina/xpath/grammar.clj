(ns suina.xpath.grammar
  (:gen-class)
  (:require [clojure.spec :as s]
            [clojure.string :as str]))

(defn- nselect-keys
  "'Nested' select-keys in a hierarchical map structure. Returns at most one match for each of the keys."
  [m keys]
  (->> m
       (tree-seq map? #(-> % vals flatten))
       (filter map?)
       (apply merge-with (fn [keep _] keep))
       (#(select-keys % keys))))

(defn- nget
  "'Nested' get in a hierarchical map structure. Returns at most one match for the key k."
  [m k]
  (-> m (nselect-keys [k]) (get k)))


(defn- ncollect
  [m k]
  (->> (tree-seq map? #(-> % vals flatten) m)
       (filter map?)
       (map k)))

(defn- genkw []
  (keyword (gensym)))


(defmacro pp [spec fn]
  "Matches spec and calls (fn parsed) on success."
  `(s/& ~spec
        (s/conformer ~fn)))

(defmacro ppc [spec c]
  "Matches spec and returns the constant value c on success."
  (let [x# (gensym)]
    `(pp ~spec (fn [~x#] ~c))))

(defmacro todo [spec]
  "Throws an exception as reminder of unimplemented (or partially implemented) rules."
  (let [p# (gensym)]
    `(pp ~spec
         (fn [~p#] (throw (UnsupportedOperationException. "TODO"))))))


(defmacro ws
  "Matches spec optionally preceded/succeeded by whitespace characters.
  The value of ws-spec determines where whitespace is allowed: :before, :after,
  :both (the default)."
  ([spec] `(ws ~spec :both))
  ([spec ws-spec]
   (if (= :ws-none ws-spec)
     `~spec
     `(pp (s/cat ~@(if (#{:both :ws-before} ws-spec)
                     [:ws-before `(s/? ::S)])
                 :body ~spec
                 ~@(if (#{:both :ws-after} ws-spec)
                     [:ws-after `(s/? ::S)]))
          #(:body %)))))

(defmacro lit [l & ws-spec]
  "Matches a char/string literal."
  (if (char? l)
    `(ws #{~l} ~ws-spec)
    (let [keywords# (repeatedly genkw)
          sets# (map (fn [c] #{c}) (seq l))]
      `(ws (pp (s/cat ~@(interleave keywords# sets#))
               #(-> % vals str/join)) ~ws-spec))
    )
  )

;; simple choices: X | Y | Z | ...
(defmacro simple-choice [& specs]
  "Matches one of the provided specs."
  (let [keywords (repeatedly genkw)
        pairs# (interleave keywords specs)]
    `(pp (s/alt ~@pairs#)
         #(second %))))

;; simple 1-or-more SEP-separated lists: X (SEP X)*
(defmacro separated-list-of [spec separator-spec]
  "Matches a sequence of one or more spec separated by separator-spec."
  (let [s# (genkw)
        rest# (genkw)
        sep# (genkw)]
    `(pp (s/cat ~s# ~spec
                ~rest# (s/* (s/cat ~sep# ~separator-spec
                                   ~s# ~spec)))
         #(ncollect % ~s#))))

;; simple 1-or-more comma-separated lists: X ("," X)*
(defmacro comma-separated-list-of [spec]
  "Matches a sequence of one or more spec separated by a comma."
  `(separated-list-of ~spec (lit \,)))


(defmacro axis [name]
  (let [kw1# (genkw)
        kv2# (genkw)]
    `(s/cat ~kw1# (lit ~name :ws-none)
            ~kv2# (lit "::" :ws-none))))

;;; some helper specs
(s/def ::_wildcard
  (pp (lit \*)
      (fn [_] {:uri \* :local \*})))

(defrecord ASTNode [type data children])

(defn ast-node [type & [data & children]]
  (ASTNode. type data children))


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
                                 :_c1 (lit \: :ws-none)
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
(s/def ::Comment (pp (s/cat :_c1 (lit "(:" :ws-before)
                            :contents ::CommentContents
                            :_c2 (lit ":)" :ws-after))
                     #(ast-node :comment (:contents %))))

;; [120] EscapeApos ::= "''"
(s/def ::EscapeApos (ppc (lit "''" :ws-none)
                         \'))

;; [119] EscapeQuot ::= '""'
(s/def ::EscapeQuot (ppc (lit "\"\"" :ws-none)
                         \"))

;; [118] BracedURILiteral ::= "Q" "{" [^{}]* "}"
(s/def ::BracedURILiteral-nonWS (pp (s/cat :_c1 (lit "Q{" :ws-none)
                                           :uri (s/* #(xmlchar? % #{\{ \}}))
                                           :_c2  (lit \} :ws-none))
                                    #(->> % :uri (apply str))))

(s/def ::BracedURILiteral (ws ::BracedURILiteral-nonWS))

;; [117] URIQualifiedName ::= BracedURILiteral NCName
(s/def ::URIQualifiedName (ws (s/cat :uri ::BracedURILiteral-nonWS
                                     :local ::NCName-nonWS)))

;; [116] StringLiteral::= ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
(s/def ::StringLiteral (pp (simple-choice (s/cat :_c1 (lit \" :ws-before)
                                                 :str (s/* (simple-choice ::EscapeQuot #(xmlchar? % #{\"})))
                                                 :_c2 (lit \" :ws-after))
                                          (s/cat :_c1 (lit \' :ws-before)
                                                 :str (s/* (simple-choice ::EscapeApos #(xmlchar? % #{\'})))
                                                 :_c2 (lit \' :ws-after)))
                           (fn [x] (->> x :str (apply str)))))

;; [115] DoubleLiteral ::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
(s/def ::DoubleLiteral (ws (pp (s/cat :base (simple-choice (s/cat :_c1 (lit \. :ws-none)
                                                                  :frac ::Digits)
                                                           (s/cat :int ::Digits
                                                                  :_pf (s/? (s/cat :_c1 (lit \. :ws-none)
                                                                                   :frac (s/* digit?)))))
                                      :_c1 #{\e \E}
                                      :sign (s/? #{\+ \-})
                                      :exp ::Digits)
                               (fn [x]
                                 (let [iraw (nget x :int)
                                       i (if (nil? iraw) \0 (apply str iraw))
                                       fraw (nget x :frac)
                                       f (if (nil? fraw) \0 (apply str fraw))
                                       s (or (:sign x) \+)
                                       e (->> x :exp (apply str))
                                       all (str i \. f \E s e)]
                                   (read-string all))))))


;; [114] DecimalLiteral ::= ("." Digits) | (Digits "." [0-9]*)
(s/def ::DecimalLiteral (ws (pp (simple-choice (s/cat :_c1 (lit \. :ws-none)
                                                      :frac ::Digits)
                                               (s/cat :int ::Digits
                                                      :_c1 (lit \. :ws-none)
                                                      :frac (s/* digit?)))
                                (fn [x]
                                  (let [iraw (nget x :int)
                                        i (if (nil? iraw) \0 (apply str iraw))
                                        fraw (nget x :frac)
                                        f (if (nil? fraw) \0 (apply str fraw))
                                        all (str i \. f \M)]
                                    (read-string all))))))

;; [113] IntegerLiteral ::= Digits
(s/def ::IntegerLiteral (ws (pp ::Digits
                                #(->> % (apply str) read-string))))

;; [112] EQName ::= QName | URIQualifiedName
(s/def ::EQName (simple-choice ::QName ::URIQualifiedName))

;; [111] ParenthesizedItemType ::= "(" ItemType ")"
(s/def ::ParenthesizedItemType (pp (s/cat :_c1 (lit \()
                                          :type ::ItemType
                                          :_c2 (lit \)))
                                   #(:type %)))

;; [110] TypedArrayTest ::= "array" "(" SequenceType ")"
(s/def ::TypedArrayTest (todo (s/cat :_c1 (lit "array")
                                     :_c2 (lit \()
                                     :seqtype ::SequenceType
                                     :_c3 (lit \)))))

;; [109] AnyArrayTest ::= "array" "(" "*" ")"
(s/def ::AnyArrayTest (ppc (s/cat :_c1 (lit "array")
                                   :_c2 (lit \()
                                   :_c3 (lit \*)
                                   :_c4 (lit \)))
                          (ast-node :any-array-test) ))


;; [108] ArrayTest ::= AnyArrayTest | TypedArrayTest
(s/def ::ArrayTest (simple-choice ::AnyArrayTest ::TypedArrayTest))

;; [107] TypedMapTest ::= "map" "(" AtomicOrUnionType "," SequenceType ")"
(s/def ::TypedMapTest (todo (s/cat :_c1 (lit "map")
                                   :_c2 (lit \()
                                   :autype ::AtomicOrUnionType
                                   :_c3 (lit \,)
                                   :seqtype ::SequenceType
                                   :_c4 (lit \)))))

;; [106] AnyMapTest ::= "map" "(" "*" ")"
(s/def ::AnyMapTest (ppc (s/cat :_c1 (lit "map")
                                 :_c2 (lit \()
                                 :_c3 (lit \*)
                                 :_c4 (lit \)))
                         (ast-node :any-map-test)))

;; [105] MapTest ::= AnyMapTest | TypedMapTest
(s/def ::MapTest (simple-choice ::AnyMapTest ::TypedMapTest))

;; [104] TypedFunctionTest ::= "function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType
(s/def ::TypedFunctionTest (todo (s/cat :_c1 (lit "function")
                                        :_c2 (lit \()
                                        :argtypes (s/? (comma-separated-list-of ::SequenceType))
                                        :_c3 (lit "as")
                                        :returntype ::SequenceType)))

;; [103] AnyFunctionTest ::= "function" "(" "*" ")"
(s/def ::AnyFunctionTest (ppc (s/cat :_c1 (lit "function")
                                     :_c2 (lit \()
                                     :_c3 (lit \*)
                                     :_c4 (lit \)))
                              (ast-node :any-fn-test)))

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
(s/def ::SchemaElementTest (pp (s/cat :_c1 (lit "schema-element")
                                      :_c2 (lit \()
                                      :element ::ElementDeclaration
                                      :_c3 (lit \)))
                               #(ast-node :schema-element-test (nselect-keys % [:element]))))

;; [95] ElementNameOrWildcard ::= ElementName | "*"
(s/def ::ElementNameOrWildcard (simple-choice ::ElementName
                                              ::_wildcard))

;; [94] ElementTest ::= "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
(s/def ::ElementTest (pp (s/cat :_c1 (lit "element")
                                :_c2 (lit \()
                                :args (s/? (s/cat :name ::ElementNameOrWildcard
                                                  :rest (s/? (s/cat :_c1 (lit \,)
                                                                    :type ::TypeName
                                                                    :q (s/? (lit \?))))))
                                :_c3 (lit \)))
                         #(ast-node :element-test (nselect-keys % [:name :type :q]))))

;; [93] AttributeDeclaration ::= AttributeName
(s/def ::AttributeDeclaration ::AttributeName)

;; [92] SchemaAttributeTest ::= "schema-attribute" "(" AttributeDeclaration ")"
(s/def ::SchemaAttributeTest (pp (s/cat :_c1 (lit "schema-attribute")
                                        :_c2 (lit \()
                                        :attribute ::AttributeDeclaration
                                        :_c3 (lit \)))
                                 #(ast-node :schema-attribute-test (nselect-keys % [:attribute]))))

;; [91] AttribNameOrWildcard ::= AttributeName | "*"
(s/def ::AttribNameOrWildcard (simple-choice ::AttributeName
                                             ::_wildcard))

;; [90] AttributeTest ::= "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
(s/def ::AttributeTest (pp (s/cat :_c1 (lit "attribute")
                                 :_c2 (lit \()
                                 :args (s/? (s/cat :name ::AttribNameOrWildcard
                                                   :rest (s/? (s/cat :_c1 (lit \,)
                                                                     :type ::TypeName))))
                                 :_c3 (lit \)))
                          #(ast-node :attribute-test (nselect-keys % [:name :type]))))

;; [89] PITest ::= "processing-instruction" "(" (NCName | StringLiteral)? ")"
(s/def ::PITest (pp (s/cat :_c1 (lit "processing-instruction")
                           :_c2 (lit \()
                           :target (s/? (simple-choice ::NCName ::StringLiteral))
                           :_c3 (lit \)))
                    #(ast-node :pi-test (nselect-keys % [:target]))))

;; [88] NamespaceNodeTest ::= "namespace-node" "(" ")"
(s/def ::NamespaceNodeTest (ppc (s/cat :_c1 (lit "namespace-node")
                                        :_c2 (lit \()
                                        :_c3 (lit \)))
                                (ast-node :namespace-node-test)))

;; [87] CommentTest ::= "comment" "(" ")"
(s/def ::CommentTest (ppc (s/cat :_c1 (lit "comment")
                                  :_c2 (lit \()
                                  :_c3 (lit \)))
                          (ast-node :comment-test)))

;; [86] TextTest ::= "text" "(" ")"
(s/def ::TextTest (ppc (s/cat :_c1 (lit "text")
                               :_c2 (lit \()
                               :_c3 (lit \)))
                       (ast-node :text-test)))

;; [85] DocumentTest ::= "document-node" "(" (ElementTest | SchemaElementTest)? ")"
(s/def ::DocumentTest (pp (s/cat :_c1 (lit "document-node")
                                   :_c2 (lit \()
                                   :element (s/? (simple-choice ::ElementTest ::SchemaElementTest))
                                   :_c3 (lit \)))
                          #(ast-node :document-test (nselect-keys % [:element]))))

;; [84] AnyKindTest ::= "node" "(" ")"
(s/def ::AnyKindTest (ppc (s/cat :_c1 (lit "node")
                                  :_c2 (lit \()
                                  :_c3 (lit \)))
                           (ast-node :any-kind-test)))



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
(s/def ::_AnyItemTest (ppc (s/cat :_c1 (lit "item")  ; helper
                                  :_c2 (lit \()
                                  :_c3 (lit \)))
                           (ast-node :any-item-test)))

(s/def ::ItemType (simple-choice ::KindTest ::_AnyItemTest ::FunctionTest ::MapTest
                                 ::ArrayTest ::AtomicOrUnionType ::ParenthesizedItemType))

;; [80] OccurrenceIndicator ::= "?" | "*" | "+"
(s/def ::OccurrenceIndicator (simple-choice (lit \?) (lit \*) (lit \+)))

;; [79] SequenceType ::= ("empty-sequence" "(" ")") | (ItemType OccurrenceIndicator?)
(s/def ::_EmptySequence (todo (s/cat :_c1 (lit "empty-sequence")  ;;helper
                                     :_c2 (lit \()
                                     :_c3 (lit \)))))

(s/def ::SequenceType (simple-choice ::_EmptySequence
                                     (s/cat :type ::ItemType
                                            :occur (s/? ::OccurrenceIndicator))))

;; [78] TypeDeclaration ::= "as" SequenceType
(s/def ::TypeDeclaration (s/cat :_c1 (lit "as")
                                :type ::SequenceType))

;; [77] SingleType ::= SimpleTypeName "?"?
(s/def ::SingleType (s/cat :type ::SimpleTypeName
                           :opt (s/? (lit \?))))

;; [76] UnaryLookup ::= "?" KeySpecifier
(s/def ::UnaryLookup (s/cat :_c1 (lit \?)
                            :ks ::KeySpecifier))

;; [75] CurlyArrayConstructor ::= "array" "{" Expr? "}"
(s/def ::CurlyArrayConstructor (s/cat :_c1 (lit "array")
                                      :_c2 (lit \{)
                                      :expr (s/? ::Expr)
                                      :_c3 (lit \))))

;; [74] SquareArrayConstructor ::= "[" (ExprSingle ("," ExprSingle)*)? "]"
(s/def ::SquareArrayConstructor (s/cat :_c1 (lit \[)
                                       :ex1 (s/? (comma-separated-list-of ::ExprSingle))
                                       :_c2 (lit \])))


;; [73] ArrayConstructor ::= SquareArrayConstructor | CurlyArrayConstructor
(s/def ::ArrayConstructor (simple-choice ::SquareArrayConstructor ::CurlyArrayConstructor))

;; [72] MapValueExpr ::= ExprSingle
(s/def ::MapValueExpr ::ExprSingle)

;; [71] MapKeyExpr ::= ExprSingle
(s/def ::MapKeyExpr ::ExprSingle)

;; [70] MapConstructorEntry ::= MapKeyExpr ":" MapValueExpr
(s/def ::MapConstructorExpr (s/cat :key ::MapKeyExpr
                                   :_c1 (lit \:)
                                   :value ::MapValueExpr))

;; [69] MapConstructor ::= "map" "{" (MapConstructorEntry ("," MapConstructorEntry)*)? "}"
(s/def ::MapConstructor (s/cat :_c1 (lit "map")
                               :_c2 (lit \{)
                               :ex1 (s/? (comma-separated-list-of ::MapConstructorEntry))
                               :_c3 (lit \))))

;; [68] InlineFunctionExpr ::= "function" "(" ParamList? ")" ("as" SequenceType)? FunctionBody
(s/def ::InlineFunctionExpr (s/cat :_c1 (lit "function")
                                   :_c2 (lit \()
                                   :params (s/? ::ParamList)
                                   :_c3 (lit \))
                                   :rtype (s/? (s/cat :_c1 (lit "as")
                                                      :type ::SequenceType))
                                   :body ::FunctionBody))

;; [67] NamedFunctionRef ::= EQName "#" IntegerLiteral
(s/def ::NamedFunctionRef (s/cat :name ::EQname
                                 :_c1 (lit \#)
                                 :arity ::IntegerLiteral))

;; [66] FunctionItemExpr ::= NamedFunctionRef | InlineFunctionExpr
(s/def ::FunctionItemExpr (simple-choice ::NamedFunctionRef ::InlineFunctionExpr))

;; [65] ArgumentPlaceholder ::= "?"
(s/def ::ArgumentPlaceholder (lit \?))

;; [64] Argument ::= ExprSingle | ArgumentPlaceholder
(s/def ::Argument (simple-choice ::ExprSingle ::ArgumentPlaceholder))

;; [63] FunctionCall ::= EQName ArgumentList
(s/def ::FunctionCall (s/cat :name ::EQName
                             :args ::ArgumentList))

;; [62] ContextItemExpr ::= "."
(s/def ::ContextItemExpr (lit \.))

;; [61] ParenthesizedExpr ::= "(" Expr? ")"
(s/def ::ParenthesizedExpr (s/cat :_c1 (lit \()
                                  :expr (s/? ::Expr)
                                  :_c2 (lit \))))

;; [60] VarName ::= EQName
(s/def ::VarName ::EQName)

;; [59] VarRef ::= "$" VarName
(s/def ::VarRef (s/cat :_c1 (lit \$)
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
(s/def ::ArrowPostfix (s/cat :_c1 (lit "=>")
                             :spec ::ArrowFunctionSpecifier
                             :args ::ArgumentList))

;; [53] KeySpecifier ::= NCName | IntegerLiteral | ParenthesizedExpr | "*"
(s/def ::KeySpecifier (simple-choice ::NCName ::IntegerLiteral ::ParenthesizedExpr (lit \*)))

;; [52] Lookup ::= "?" KeySpecifier
(s/def ::Lookup (s/cat :_c1 (lit \?)
                       :spec ::KeySpecifier))

;; [51] Predicate ::= "[" Expr "]"
(s/def ::Predicate (s/cat :_c1 (lit \[)
                          :expr ::Expr
                          :_c1 (lit \])))

;; [50] PredicateList ::= Predicate*
(s/def ::PredicateList (s/* ::Predicate))

;; [49] ArgumentList ::= "(" (Argument ("," Argument)*)? ")"
(s/def ::ArgumentList (s/cat :_c1 (lit \()
                             :args (comma-separated-list-of ::Argument)
                             :_c2 (lit \))))

;; [48] PostfixExpr ::= PrimaryExpr (Predicate | ArgumentList | Lookup | ArrowPostfix)*
(s/def ::PostfixExpr (s/cat :expr ::PrimaryExpr
                            :rest (simple-choice ::Predicate ::ArgumentList ::Lookup ::ArrowPostfix)))

;; [47] Wildcard ::= "*" | (NCName ":" "*") | ("*" ":" NCName) | (BracedURILiteral "*") /* ws: explicit */
(s/def ::Wildcard (pp (simple-choice ::_wildcard
                                     (s/cat :prefix ::NCName
                                            :_c1 (lit \: :ws-none)
                                            :local (lit \* :ws-after))
                                     (s/cat :prefix (lit \* :ws-before)
                                            :_c1 (lit \: :ws-none)
                                            :local ::NCName)
                                     (s/cat :uri ::BracedURILiteral
                                            :local (lit \* :ws-after)))
                      #(if (map? %) (select-keys % [:uri :local :prefix]) %)))

;; [46] NameTest ::= EQName | Wildcard
(s/def ::NameTest (simple-choice ::EQName ::Wildcard))

;; [45] NodeTest ::= KindTest | NameTest
(s/def ::NodeTest (simple-choice ::KindTest ::NameTest))

;; [44] AbbrevReverseStep ::= ".."
(s/def ::AbbrevReverseStep (lit ".."))

;; [43] ReverseAxis ::= ("parent" "::") | ("ancestor" "::") | ("preceding-sibling" "::") | ("preceding" "::") | ("ancestor-or-self" "::")
(s/def ::ReverseAxis (simple-choice (axis "parent") (axis "preceding-sibling")
                                    (axis "preceding") (axis "ancestor-or-self")))

;; [42] ReverseStep ::= (ReverseAxis NodeTest) | AbbrevReverseStep
(s/def ::ReverseStep (simple-choice (s/cat :axis ::ReverseAxis
                                           :nodetest ::NodeTest)
                                    ::AbbrevReverseStep))

;; [41] AbbrevForwardStep ::= "@"? NodeTest
(s/def ::AbbrevForwardStep (s/cat :attr? (lit \@)
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
(s/def ::RelativePathExpr (separated-list-of ::StepExpr (simple-choice (lit \/) (lit "//"))))

;; [35] PathExpr ::= ("/" RelativePathExpr?) | ("//" RelativePathExpr) | RelativePathExpr /* xgc: leading-lone-slash */
;; FIXME
(s/def ::PathExpr (simple-choice (s/cat :_c1 (lit \/)
                                        :pathexpr (s/? ::RelativePathExpr))
                                 (s/cat :_c1 (lit "//")
                                        :pathexpr ::RelativePathExpr)
                                 :pathexpr ::RelativePathExpr))

;; [34] SimpleMapExpr ::= PathExpr ("!" PathExpr)*
(s/def ::SimpleMapExpr (separated-list-of ::PathExpr (lit \!)))

;; [33] NodeComp ::= "is" | "<<" | ">>"
(s/def ::NodeComp (simple-choice (lit "is")
                                 (lit "<<")
                                 (lit ">>")))

;; [32] ValueComp ::= "eq" | "ne" | "lt" | "le" | "gt" | "ge"
(s/def ::ValueComp (simple-choice (lit "eq")
                                  (lit "ne")
                                  (lit "lt")
                                  (lit "le")
                                  (lit "gt")
                                  (lit "ge")))

;; [31] GeneralComp ::= "=" | "!=" | "<" | "<=" | ">" | ">="
(s/def ::GeneralComp (simple-choice (lit \=)
                                    (lit "!=")
                                    (lit \<)
                                    (lit "<=")
                                    (lit \>)
                                    (lit ">=")))

;; [30] ValueExpr ::= SimpleMapExpr
(s/def ::ValueExpr ::SimpleMapExpr)

;; [29] UnaryExpr ::= ("-" | "+")* ValueExpr
(s/def ::UnaryExpr (s/cat :signs (simple-choice (lit \-) (lit \+))
                          :expr ::ValueExpr))

;; [28] CastExpr ::= UnaryExpr ( "cast" "as" SingleType )?
(s/def ::CastExpr (s/cat :expr ::UnaryExpr
                         :cast (s/? (s/cat :_c1 (lit "cast")
                                           :_c2 (lit "as")
                                           :type ::SingleType))))

;; [27] CastableExpr ::= CastExpr ( "castable" "as" SingleType )?
(s/def ::CastableExpr (s/cat :expr ::CastExpr
                             :castable (s/? (s/cat :_c1 (lit "castable")
                                                   :_c2 (lit "as")
                                                   :type ::SingleType))))

;; [26] TreatExpr ::= CastableExpr ( "treat" "as" SequenceType )?
(s/def ::TreatExpr (s/cat :expr ::CastableExpr
                          :treat (s/? (s/cat :_c1 (lit "treat")
                                             :_c2 (lit "as")
                                             :type ::SequenceType))))

;; [25] InstanceofExpr ::= TreatExpr ( "instance" "of" SequenceType )?
(s/def ::InstanceofExpr (s/cat :expr ::TreatExpr
                               :instance (s/? (s/cat :_c1 (lit "instance")
                                                     :_c2 (lit "of")
                                                     :type ::SequenceType))))

;; [24] IntersectExceptExpr ::= InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
(s/def ::IntersectExceptExpr (separated-list-of ::InstanceofExpr (simple-choice (lit "intersect")
                                                                                (lit "except"))))

;; [23] UnionExpr ::= IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
(s/def ::UnionExpr (separated-list-of ::IntersectExceptExpr (simple-choice (lit "union")
                                                                           (lit \|))))

;; [22] MultiplicativeExpr ::= UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
(s/def ::MultiplicativeExpr (separated-list-of ::UnionExpr (simple-choice (lit \*)
                                                                          (lit "div")
                                                                          (lit "idiv")
                                                                          (lit "mode"))))

;; [21] AdditiveExpr ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
(s/def ::AdditiveExpr (separated-list-of ::MultiplicativeExpr (simple-choice (lit \+)
                                                                             (lit \-))))

;; [20] RangeExpr ::= AdditiveExpr ( "to" AdditiveExpr )?
(s/def ::RangeExpr (s/cat :expr ::AdditiveExpr
                          :to (s/? (s/cat :_c1 (lit "to")
                                          :expr-to ::AdditiveExpr))))

;; [19] StringConcatExpr ::= RangeExpr ( "||" RangeExpr )*
(s/def ::StringConcatExpr (separated-list-of ::RangeExpr (lit "||")))

;; [18] ComparisonExpr ::= StringConcatExpr ( (ValueComp | GeneralComp | NodeComp) StringConcatExpr )?
(s/def ::ComparisonExpr (s/cat :expr ::StringConcatExpr
                               :comp (s/? (s/cat :comp (simple-choice ::ValueComp ::GeneralComp ::NodeComp)
                                                 :expr ::StringConcatExpr))))

;; [17] AndExpr ::= ComparisonExpr ( "and" ComparisonExpr )*
(s/def ::AndExpr (separated-list-of ::ComparisonExpr (lit "and")))

;; [16] OrExpr ::= AndExpr ( "or" AndExpr )*
(s/def ::OrExpr (separated-list-of ::AndExpr (lit "or")))

;; [15] IfExpr ::= "if" "(" Expr ")" "then" ExprSingle "else" ExprSingle
(s/def ::IfExpr (s/cat :_c1 (lit "if")
                       :_c2 (lit \()
                       :expr ::Expr
                       :_c3 (lit \))
                       :_c4 (lit "then")
                       :then ::ExprSingle
                       :_c5 (lit "else")
                       :else ::ExprSingle))

;; [14] QuantifiedExpr ::= ("some" | "every") "$" VarName "in" ExprSingle
;;                         ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle
(s/def ::QuantifiedExpr (s/cat :some-every (simple-choice (lit "some") (lit "every"))
                               :_c1 (lit \$)
                               :bindings (comma-separated-list-of (s/cat :var ::VarName
                                                                         :_c1 (lit "in")
                                                                         :expr ::ExprSingle))
                               :_c2 (lit "satisfies")
                               :expr ::ExprSingle))

;; [13] SimpleLetBinding ::= "$" VarName ":=" ExprSingle
(s/def ::SimpleForBinding (s/cat :_c1 (lit \$)
                                 :var ::VarName
                                 :_c2 (lit ":=")
                                 :expr ::ExprSingle))

;; [12] SimpleLetClause ::= "let" SimpleLetBinding ("," SimpleLetBinding)*
(s/def ::SimpleLetClause (s/cat :_c1 (lit "let")
                                :bindings (comma-separated-list-of ::SimpleLetBinding)))

;; [11] LetExpr ::= SimpleLetClause "return" ExprSingle
(s/def ::LetExpr (s/cat :let ::SimpleLetClause
                        :_c1 (lit "return")
                        :expr ::ExprSingle))

;; [10] SimpleForBinding ::= "$" VarName "in" ExprSingle
(s/def ::SimpleForBinding (s/cat :_c1 (lit \$)
                                 :var ::VarName
                                 :_c2 (lit "in")
                                 :expr ::ExprSingle))

;; [9] SimpleForClause ::= "for" SimpleForBinding ("," SimpleForBinding)*
(s/def ::SimpleForClause (s/cat :_c1 (lit "for")
                                :bindings (comma-separated-list-of ::SimpleForBinding)))

;; [8] ForExpr ::= SimpleForClause "return" ExprSingle
(s/def ::ForExpr (s/cat :sfc ::SimpleForClause
                        :_c1 (lit "return")
                        :expr ::ExprSingle))

;; [7] ExprSingle ::= ForExpr | LetExpr | QuantifiedExpr | IfExpr | OrExpr
(s/def ::ExprSingle (simple-choice ::ForExpr ::LetExpr ::QuantifiedExpr ::IfExpr ::OrExpr))

;; [6] Expr ::= ExprSingle ("," ExprSingle)*
(s/def ::Expr (comma-separated-list-of ::ExprSingle))

;; [5] EnclosedExpr ::= "{" Expr "}"
(s/def ::EnclosedExpr (s/cat :_c1 (lit \{)
                             :expr ::Expr
                             :_c2 (lit \))))

;; [4] FunctionBody ::= EnclosedExpr
(s/def ::FunctionBody ::EnclosedExpr)

;; [3] Param ::= "$" EQName TypeDeclaration?
(s/def ::Param (s/cat :_c1 (lit \$)
                      :name ::EQName
                      :type (s/? ::TypeDeclaration)))

;; [2] ParamList ::= Param ("," Param)*
(s/def ::ParamList (comma-separated-list-of ::Param))

;; [1] XPath ::= Expr
(s/def ::XPath ::Expr)


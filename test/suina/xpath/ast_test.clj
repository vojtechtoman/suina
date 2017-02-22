(ns suina.xpath.ast-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [suina.xml :as xml]
            [suina.xpath.grammar :as xpathg]))

(defn- makeseq [x]
  (if (sequential? x) x (seq x)))

(defn parse
  ([expr] (parse ::xpathg/XPath expr))
  ([spec expr] (parse spec expr true))
  ([spec expr makeseq?]
   (let [s (if makeseq? (makeseq expr) expr)]
     (s/conform spec s))))

(defn explain
  ([expr] (explain ::xpathg/XPath expr))
  ([spec expr] (explain spec expr true))
  ([spec expr makeseq?]
   (let [s (if makeseq? (makeseq expr) expr)]
     (s/explain-str spec s))))

(defn parses? [spec expr]
  (let [res (parse spec expr)]
    (not= :clojure.spec/invalid res)))

(deftest check-ast-char
  (testing "checks the constructed ASTs for the XML/XPath production rules (various character classes)"
    (are [ast spec expr] (= ast (parse spec expr false))
      ;; [xml:2]
      \a ::xpathg/Char \a
      ;; [xml:4]
      \A ::xpathg/NameStartChar \A
      \a ::xpathg/NameStartChar \a
      \: ::xpathg/NameStartChar \:
      \_ ::xpathg/NameStartChar \_
      ;; [xml:4a]
      \A ::xpathg/NameChar \A
      \a ::xpathg/NameChar \a
      \: ::xpathg/NameChar \:
      \_ ::xpathg/NameChar \_
      \- ::xpathg/NameChar \-
      \. ::xpathg/NameChar \.
      \0 ::xpathg/NameChar \0)))

(deftest check-ast
  (testing "checks the constructed ASTs for the XML/XPath production rules"
    (are [ast spec expr] (= ast (parse spec expr))
      ;; [xml:3]
      [\space] ::xpathg/S [\space]
      [\tab] ::xpathg/S [\tab]
      [\return] ::xpathg/S [\return]
      [\newline] ::xpathg/S [\newline]
      [\space \space \tab \return \newline] ::xpathg/S [\space \space \tab \return \newline]
      ;; [:xml5]
      ":a:_-.0" ::xpathg/Name ":a:_-.0"
      ":" ::xpathg/Name ":"
      "foo" ::xpathg/Name "foo"
      "foo8:" ::xpathg/Name "foo8:"
      ;; [xmlns:4]
      "foo" ::xpathg/NCName "foo"
      "foo" ::xpathg/NCName " foo "
      ;; [xmlns:7
      {:prefix "foo" :local "bar"}::xpathg/QName "foo:bar"
      {:local "foo"}::xpathg/QName "foo"
      ;; [xmlns:8]
      {:prefix "foo" :local "bar"} ::xpathg/PrefixedName "foo:bar"
      ;; [xmlns:9]
      {:local "foo"} ::xpathg/UnprefixedName "foo"
      ;; [xmlns:10]
      "foo" ::xpathg/Prefix "foo"
      ;; [xmlns:11]
      "foo" ::xpathg/LocalPart "foo"
      ;; [126]
      "fooX" ::xpathg/CommentContents "fooX"
      ;; [125]
      [\1 \2 \3] ::xpathg/Digits "123"
      ;; [121]
      (xpathg/ast-node :comment "fooX") ::xpathg/Comment "(:fooX:)"
      ;; [120]
      \' ::xpathg/EscapeApos "''"
      ;; [119]
      \" ::xpathg/EscapeQuot "\"\""
      ;; [118]
      "foo" ::xpathg/BracedURILiteral "Q{foo}"
      ;; [117]
      {:uri "foo" :local "bar"} ::xpathg/URIQualifiedName "Q{foo}bar"
      ;; [116]
      "foo" ::xpathg/StringLiteral "\"foo\""
      "f\"oo" ::xpathg/StringLiteral "\"f\"\"oo\""
      "f\"oo" ::xpathg/StringLiteral "'f\"oo'"
      "foo" ::xpathg/StringLiteral "'foo'"
      "f'oo" ::xpathg/StringLiteral "'f''oo'"
      ;; [115]
      1E10 ::xpathg/DoubleLiteral "1.0E10"
      1E10 ::xpathg/DoubleLiteral "1.0e10"
      1E9 ::xpathg/DoubleLiteral ".1E10"
      1E9 ::xpathg/DoubleLiteral ".1e10"
      1E-10 ::xpathg/DoubleLiteral "1E-10"
      1E-11 ::xpathg/DoubleLiteral ".1E-10"
      1E9 ::xpathg/DoubleLiteral ".1E+10"
      1.023E11 ::xpathg/DoubleLiteral "10.23E+10"
      ;; [114]
      0.123M ::xpathg/DecimalLiteral ".123"
      123M ::xpathg/DecimalLiteral "123."
      123.456M ::xpathg/DecimalLiteral "123.456"
      ;; [113]
      123 ::xpathg/IntegerLiteral "123"
      ;; [112]
      {:prefix "foo" :local "bar"} ::xpathg/EQName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/EQName "Q{foo}bar"
      ;; [110]
      (xpathg/ast-node :any-array-test) ::xpathg/AnyArrayTest "array(*)"
      ;; [106]
      (xpathg/ast-node :any-map-test) ::xpathg/AnyMapTest "map(*)"
      ;; [103]
      (xpathg/ast-node :any-fn-test) ::xpathg/AnyFunctionTest "function(*)"
      ;; [101]
      {:prefix "foo" :local "bar"} ::xpathg/TypeName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/TypeName "Q{foo}bar"
      ;; [100]
      {:prefix "foo" :local "bar"} ::xpathg/SimpleTypeName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/SimpleTypeName "Q{foo}bar"
      ;; [99]
      {:prefix "foo" :local "bar"} ::xpathg/ElementName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/ElementName "Q{foo}bar"
      ;; [98]
      {:prefix "foo" :local "bar"} ::xpathg/AttributeName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/AttributeName "Q{foo}bar"
      ;; [97]
      {:prefix "foo" :local "bar"} ::xpathg/ElementDeclaration "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/ElementDeclaration "Q{foo}bar"
      ;; [96]
      (xpathg/ast-node :schema-element-test {:element {:prefix "foo" :local "bar"}}) ::xpathg/SchemaElementTest "schema-element(foo:bar)"
      ;; [95]
      {:prefix "foo" :local "bar"} ::xpathg/ElementNameOrWildcard "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/ElementNameOrWildcard "Q{foo}bar"
      {:uri \* :local \*} ::xpathg/ElementNameOrWildcard "*"
      ;; [94]
      (xpathg/ast-node :element-test {:name {:prefix "foo" :local "bar"}}) ::xpathg/ElementTest "element(foo:bar)"
      (xpathg/ast-node :element-test {:name {:uri \* :local \*}}) ::xpathg/ElementTest "element(*)"
      (xpathg/ast-node :element-test {:name {:prefix "foo" :local "bar"} :type {:prefix "baz" :local "span"} :q \?}) ::xpathg/ElementTest "element(foo:bar, baz:span?)"
      ;; [93]
      {:prefix "foo" :local "bar"} ::xpathg/AttributeDeclaration "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/AttributeDeclaration "Q{foo}bar"
      ;; [92]
      (xpathg/ast-node :schema-attribute-test {:attribute {:prefix "foo" :local "bar"}}) ::xpathg/SchemaAttributeTest "schema-attribute(foo:bar)"
      ;; [91]
      {:prefix "foo" :local "bar"} ::xpathg/AttribNameOrWildcard "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/AttribNameOrWildcard "Q{foo}bar"
      {:uri \* :local \*} ::xpathg/AttribNameOrWildcard "*"
      ;; [90]
      (xpathg/ast-node :attribute-test {:name {:prefix "foo" :local "bar"}}) ::xpathg/AttributeTest "attribute(foo:bar)"
      (xpathg/ast-node :attribute-test {:name {:uri \* :local \*}}) ::xpathg/AttributeTest "attribute(*)"
      (xpathg/ast-node :attribute-test {:name {:prefix "foo" :local "bar"} :type {:prefix "baz" :local "span"}}) ::xpathg/AttributeTest "attribute(foo:bar, baz:span)"
      ;; [89]
      (xpathg/ast-node :pi-test {:target "ncname"}) ::xpathg/PITest "processing-instruction(ncname)"
      (xpathg/ast-node :pi-test {:target "st'r"}) ::xpathg/PITest "processing-instruction('st''r')"
      ;; [88]
      (xpathg/ast-node :namespace-node-test) ::xpathg/NamespaceNodeTest "namespace-node()"
      ;; [87]
      (xpathg/ast-node :comment-test) ::xpathg/CommentTest "comment()"
      ;; [86]
      (xpathg/ast-node :text-test) ::xpathg/TextTest "text()"
      ;; [85]
      (xpathg/ast-node :document-test {}) ::xpathg/DocumentTest "document-node()"
      (xpathg/ast-node :document-test {:element (xpathg/ast-node :element-test {:name {:prefix "foo" :local "bar"}})}) ::xpathg/DocumentTest "document-node(element(foo:bar))"
      (xpathg/ast-node :document-test {:element (xpathg/ast-node :schema-element-test {:element {:prefix "foo" :local "bar"}})}) ::xpathg/DocumentTest "document-node(schema-element(foo:bar))"
      ;; [84]
      (xpathg/ast-node :any-kind-test) ::xpathg/AnyKindTest "node()"
      ;; [81]
      (xpathg/ast-node :any-item-test) ::xpathg/ItemType "item()"
      ;; [80]
      \? ::xpathg/OccurrenceIndicator "?"
      \* ::xpathg/OccurrenceIndicator "*"
      \+ ::xpathg/OccurrenceIndicator "+"
      ;; [65]
      \? ::xpathg/OccurrenceIndicator "?"
      ;; [60]
      \. ::xpathg/ContextItemExpr "."
      {:prefix "foo" :local "bar"} ::xpathg/VarName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/VarName "Q{foo}bar"
      ;; [58]
      123 ::xpathg/NumericLiteral "123"
      1E10 ::xpathg/NumericLiteral "1.0E10"
      123.456M ::xpathg/NumericLiteral "123.456"
      ;; [57]
      123 ::xpathg/Literal "123"
      1E10 ::xpathg/Literal "1.0E10"
      123.456M ::xpathg/Literal "123.456"
      "foo" ::xpathg/Literal "\"foo\""
      "foo" ::xpathg/Literal "'foo'"
      ;; [47]
      {:uri \* :local \*} ::xpathg/Wildcard "*"
      {:prefix "foo" :local \*} ::xpathg/Wildcard "foo:*"
      {:prefix \* :local "foo"} ::xpathg/Wildcard "*:foo"
      {:uri "foo" :local \*} ::xpathg/Wildcard "Q{foo}*"
      ;; [46]
      {:prefix "foo" :local "bar"} ::xpathg/NameTest "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/NameTest "Q{foo}bar"
      {:uri \* :local \*} ::xpathg/NameTest "*"
      {:prefix "foo" :local \*} ::xpathg/NameTest "foo:*"
      {:prefix \* :local "foo"} ::xpathg/NameTest "*:foo"
      {:uri "foo" :local \*} ::xpathg/NameTest "Q{foo}*")

    (deftest parse-invalid
      (testing "parsing of invalid instances with respect to the XPath grammar"
        (are [spec expr] (not (parses? spec expr))
          ::xpathg/Name " foo8:"
          ::xpathg/NCName "foo:"
          ::xpathg/PrefixedName ":bar"
          ::xpathg/QName "foo:"
          ::xpathg/QName ""
          ::xpathg/CommentContents "fo(:oX"
          ::xpathg/CommentContents "fo:)oX"
          ::xpathg/Comment "(:fo(:oX:)"
          ::xpathg/BracedURILiteral "Q {foo}"
          ::xpathg/URIQualifiedName "Q{foo} bar"
          ::xpathg/StringLiteral "\"fo\"o\""
          ::xpathg/StringLiteral "'f'oo'"
          ::xpathg/DoubleLiteral "1.0E1x"
          ::xpathg/DoubleLiteral "1"
          ::xpathg/DecimalLiteral "123..456"
          ::xpathg/IntegerLiteral "a")))))


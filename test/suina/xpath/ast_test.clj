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
      \a ::xpathg/Char \a                                                                ; [xml:2]
      \A ::xpathg/NameStartChar \A                                                       ; [xml:4]
      \a ::xpathg/NameStartChar \a
      \: ::xpathg/NameStartChar \:
      \_ ::xpathg/NameStartChar \_
      \A ::xpathg/NameChar \A                                                            ; [xml:4a]
      \a ::xpathg/NameChar \a
      \: ::xpathg/NameChar \:
      \_ ::xpathg/NameChar \_
      \- ::xpathg/NameChar \-
      \. ::xpathg/NameChar \.
      \0 ::xpathg/NameChar \0)))

(deftest check-ast
  (testing "checks the constructed ASTs for the XML/XPath production rules"
    (are [ast spec expr] (= ast (parse spec expr))
      [\space] ::xpathg/S [\space]                                                        ; [xml:3]
      [\tab] ::xpathg/S [\tab]
      [\return] ::xpathg/S [\return]
      [\newline] ::xpathg/S [\newline]
      [\space \space \tab \return \newline] ::xpathg/S [\space \space \tab \return \newline]
      ":a:_-.0" ::xpathg/Name ":a:_-.0"                                                   ; [xml:5]
      ":" ::xpathg/Name ":"
      "foo" ::xpathg/Name "foo"
      "foo8:" ::xpathg/Name "foo8:"
      "foo" ::xpathg/NCName "foo"                                                         ; [xmlns:4]
      "foo" ::xpathg/NCName " foo "
      {:prefix "foo" :local "bar"}::xpathg/QName "foo:bar"                                ; [xmlns:7]
      {:local "foo"}::xpathg/QName "foo"
      {:prefix "foo" :local "bar"} ::xpathg/PrefixedName "foo:bar"                        ; [xmlns:8]
      {:local "foo"} ::xpathg/UnprefixedName "foo"                                        ; [xmlns:9]
      "foo" ::xpathg/Prefix "foo"                                                         ; [xmlns:10]
      "foo" ::xpathg/LocalPart "foo"                                                      ; [xmlns:11]
      "fooX" ::xpathg/CommentContents "fooX"                                              ; [126]
      [\1 \2 \3] ::xpathg/Digits "123"                                                    ; [125]
      (xpathg/ast-node :comment "fooX") ::xpathg/Comment "(:fooX:)"                       ; [121]
      \' ::xpathg/EscapeApos "''"                                                         ; [120]
      \" ::xpathg/EscapeQuot "\"\""                                                       ; [119]
      "foo" ::xpathg/BracedURILiteral "Q{foo}"                                            ; [118]
      {:uri "foo" :local "bar"} ::xpathg/URIQualifiedName "Q{foo}bar"                     ; [117]
      "foo" ::xpathg/StringLiteral "\"foo\""                                              ; [116]
      "f\"oo" ::xpathg/StringLiteral "\"f\"\"oo\""
      "f\"oo" ::xpathg/StringLiteral "'f\"oo'"
      "foo" ::xpathg/StringLiteral "'foo'"
      "f'oo" ::xpathg/StringLiteral "'f''oo'"
      1E10 ::xpathg/DoubleLiteral "1.0E10"                                                ; [115]
      1E10 ::xpathg/DoubleLiteral "1.0e10"
      1E9 ::xpathg/DoubleLiteral ".1E10"
      1E9 ::xpathg/DoubleLiteral ".1e10"
      1E-10 ::xpathg/DoubleLiteral "1E-10"
      1E-11 ::xpathg/DoubleLiteral ".1E-10"
      1E9 ::xpathg/DoubleLiteral ".1E+10"
      1.023E11 ::xpathg/DoubleLiteral "10.23E+10"
      0.123M ::xpathg/DecimalLiteral ".123"                                               ; [114]
      123M ::xpathg/DecimalLiteral "123."
      123.456M ::xpathg/DecimalLiteral "123.456"
      123 ::xpathg/IntegerLiteral "123"                                                   ; [113]
      {:prefix "foo" :local "bar"} ::xpathg/EQName "foo:bar"                              ; [112]
      {:uri "foo" :local "bar"} ::xpathg/EQName "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/TypeName "foo:bar"                            ; [101]
      {:uri "foo" :local "bar"} ::xpathg/TypeName "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/SimpleTypeName "foo:bar"                      ; [100]
      {:uri "foo" :local "bar"} ::xpathg/SimpleTypeName "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/ElementName "foo:bar"                         ; [99]
      {:uri "foo" :local "bar"} ::xpathg/ElementName "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/AttributeName "foo:bar"                       ; [98]
      {:uri "foo" :local "bar"} ::xpathg/AttributeName "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/ElementDeclaration "foo:bar"                  ; [97]
      {:uri "foo" :local "bar"} ::xpathg/ElementDeclaration "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/ElementNameOrWildcard "foo:bar"               ; [95]
      {:uri "foo" :local "bar"} ::xpathg/ElementNameOrWildcard "Q{foo}bar"
      {:uri \* :local \*} ::xpathg/ElementNameOrWildcard "*"
      {:prefix "foo" :local "bar"} ::xpathg/AttributeDeclaration "foo:bar"                ; [93]
      {:uri "foo" :local "bar"} ::xpathg/AttributeDeclaration "Q{foo}bar"
      {:prefix "foo" :local "bar"} ::xpathg/AttribNameOrWildcard "foo:bar"                ; [91]
      {:uri "foo" :local "bar"} ::xpathg/AttribNameOrWildcard "Q{foo}bar"
      {:uri \* :local \*} ::xpathg/AttribNameOrWildcard "*"
      \? ::xpathg/OccurrenceIndicator "?"                                                 ; [80]
      \* ::xpathg/OccurrenceIndicator "*"
      \+ ::xpathg/OccurrenceIndicator "+"
      \? ::xpathg/OccurrenceIndicator "?"                                                 ; [65]
      \. ::xpathg/ContextItemExpr "."                                                     ; [62]
      {:prefix "foo" :local "bar"} ::xpathg/VarName "foo:bar"                             ; [60]
      {:uri "foo" :local "bar"} ::xpathg/VarName "Q{foo}bar"
      123 ::xpathg/NumericLiteral "123"                                                   ; [58]
      1E10 ::xpathg/NumericLiteral "1.0E10"
      123.456M ::xpathg/NumericLiteral "123.456"
      123 ::xpathg/Literal "123"                                                          ; [57]
      1E10 ::xpathg/Literal "1.0E10"
      123.456M ::xpathg/Literal "123.456"
      "foo" ::xpathg/Literal "\"foo\""
      "foo" ::xpathg/Literal "'foo'"
      {:uri \* :local \*} ::xpathg/Wildcard "*"                                           ; [47]
      {:prefix "foo" :local \*} ::xpathg/Wildcard "foo:*"
      {:prefix \* :local "foo"} ::xpathg/Wildcard "*:foo"
      {:uri "foo" :local \*} ::xpathg/Wildcard "Q{foo}*"
      {:prefix "foo" :local "bar"} ::xpathg/NameTest "foo:bar"                            ; [46]
      {:uri "foo" :local "bar"} ::xpathg/NameTest "Q{foo}bar"
      {:uri \* :local \*} ::xpathg/NameTest "*"
      {:prefix "foo" :local \*} ::xpathg/NameTest "foo:*"
      {:prefix \* :local "foo"} ::xpathg/NameTest "*:foo"
      {:uri "foo" :local \*} ::xpathg/NameTest "Q{foo}*")))

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
      ::xpathg/IntegerLiteral "a")))


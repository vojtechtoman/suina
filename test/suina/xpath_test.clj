(ns suina.xpath-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [suina.xpath :refer :all]
            [suina.xml.grammar :as xmlg]
            [suina.xpath.grammar :as xpathg]))

(defn parses? [rule expr]
  (let [s (seq expr)
        res (s/conform rule s)]
    (not= :clojure.spec/invalid res)))

(deftest parse-valid
  (testing "parsing of valid instances of the XPath production rules"
    (are [rule expr] (parses? rule expr)
      ::xmlg/Name "foo8:"
      ::xmlg/Name ":"
      ::xmlg/NCName "foo"
      ::xmlg/NCName " foo "
      ::xmlg/PrefixedName "foo:bar"
      ::xmlg/QName "foo:bar"
      ::xmlg/QName "bar"
      ::xpathg/CommentContents "fooX"
      ::xpathg/Comment "(:fooX:)"
      ::xpathg/EscapeApos "''"
      ::xpathg/EscapeQuot "\"\""
      ::xpathg/BracedURILiteral "Q{foo}"
      ::xpathg/URIQualifiedName "Q{foo}bar"
      ::xpathg/StringLiteral "\"foo\""
      ::xpathg/StringLiteral "\"f\"\"oo\""
      ::xpathg/StringLiteral "'f\"oo'"
      ::xpathg/StringLiteral "'foo'"
      ::xpathg/StringLiteral "'f''oo'"
      ::xpathg/DoubleLiteral "1.0E10"
      ::xpathg/DoubleLiteral "1.0e10"
      ::xpathg/DoubleLiteral ".1E10"
      ::xpathg/DoubleLiteral ".1e10"
      ::xpathg/DoubleLiteral ".1E-10"
      ::xpathg/DoubleLiteral ".1E+10"
      ::xpathg/DoubleLiteral "1E-10"
      ::xpathg/DoubleLiteral "10.23E+10"
      ::xpathg/DecimalLiteral ".123"
      ::xpathg/DecimalLiteral "123."
      ::xpathg/DecimalLiteral "123.456"
      ::xpathg/IntegerLiteral "123"
      ::xpathg/EQName "foo:bar"
      ::xpathg/EQName "Q{foo}bar")))

(deftest parse-invalid
  (testing "parsing of invalid instances with respect to the XPath grammar"
    (are [rule expr] (not (parses? rule expr))
      ::xmlg/Name " foo8:"
      ::xmlg/NCName "foo:"
      ::xmlg/PrefixedName ":bar"
      ::xmlg/QName "foo:"
      ::xmlg/QName ""
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


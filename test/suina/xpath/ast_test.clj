(ns suina.xpath.ast-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [suina.xml :as xml]
            [suina.xpath.grammar :as xpathg]))

(defn- makeseq [x]
  (if (sequential? x) x (seq x)))

(defn parse
  ([expr] (parse ::xpathg/XPath expr))
  ([spec expr]
   (let [s (makeseq expr)]
     (s/conform spec s))))

(defn explain
  ([expr] (explain ::xpathg/XPath expr))
  ([spec expr]
   (let [s (makeseq expr)]
     (s/explain-str spec s))))

(defn parses? [spec expr]
  (let [res (parse spec expr)]
    (not= :clojure.spec/invalid res)))

(deftest check-ast
  (testing "checks the constructed ASTs for the XPath production rules"
    (are [ast spec expr] (= ast (parse spec expr))
      "foo" ::xpathg/Name "foo"
      "foo8:" ::xpathg/Name "foo8:"
      "foo" ::xpathg/NCName "foo"
      "foo" ::xpathg/NCName " foo "
      {:prefix "foo" :local "bar"} ::xpathg/PrefixedName "foo:bar"
      {:prefix "foo" :local "bar"}::xpathg/QName "foo:bar"
      {:local "bar"} ::xpathg/QName "bar"
      "fooX" ::xpathg/CommentContents "fooX"
      ;;::xpathg/Comment "(:fooX:)"
      \' ::xpathg/EscapeApos "''"
      \" ::xpathg/EscapeQuot "\"\""
      "foo" ::xpathg/BracedURILiteral "Q{foo}"
      {:uri "foo" :local "bar"} ::xpathg/URIQualifiedName "Q{foo}bar"
      "foo" ::xpathg/StringLiteral "\"foo\""
      "f\"oo" ::xpathg/StringLiteral "\"f\"\"oo\""
      "f\"oo" ::xpathg/StringLiteral "'f\"oo'"
      "foo" ::xpathg/StringLiteral "'foo'"
      "f'oo" ::xpathg/StringLiteral "'f''oo'"
      1E10 ::xpathg/DoubleLiteral "1.0E10"
      1E10 ::xpathg/DoubleLiteral "1.0e10"
      1E9 ::xpathg/DoubleLiteral ".1E10"
      1E9 ::xpathg/DoubleLiteral ".1e10"
      1E-10 ::xpathg/DoubleLiteral "1E-10"
      1E-11 ::xpathg/DoubleLiteral ".1E-10"
      1E9 ::xpathg/DoubleLiteral ".1E+10"
      1.023E11 ::xpathg/DoubleLiteral "10.23E+10"
      0.123M ::xpathg/DecimalLiteral ".123"
      123M ::xpathg/DecimalLiteral "123."
      123.456M ::xpathg/DecimalLiteral "123.456"
      123 ::xpathg/IntegerLiteral "123"
      {:prefix "foo" :local "bar"} ::xpathg/EQName "foo:bar"
      {:uri "foo" :local "bar"} ::xpathg/EQName "Q{foo}bar"
      )))

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


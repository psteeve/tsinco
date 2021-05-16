(defpackage :esinco.syntaxkind
  (:use :common-lisp)
  (:export 
   :unknown
   :end-of-file-token
   :single-line-comment-trivia
   :multi-line-comment-trivia
   :new-line-trivia
   :whitespace-trivia
   :shebang-trivia
   :conflict-marker-trivia
   ;; Literals
   :numeric-literal
   :big-int-literal
   :string-literal
   :jsx-text
   :jsx-text-all-white-spaces
   :regular-expression-literal
   :no-substitution-template-literal
   ;; Pseudo-literals
   :template-head
   :template-middle
   :template-tail
   ;; Punctuation
   :open-brace-token
   :close-brace-token
   :open-paren-token
   :close-paren-token
   :open-bracket-token
   :close-bracket-token
   :dot-token
   :dot-dot-dot-token
   :semicolon-token
   :comma-token
   :less-than-token
   :less-than-slash-token
   :greater-than-token
   :less-than-equals-token
   :greater-than-equals-token
   :equals-equals-token
   :exclamation-equals-token
   :equals-equals-equals-token
   :exclamation-equals-equals-token
   :equals-greater-than-token
   :plus-token
   :minus-token
   :asterisk-token
   :asterisk-asterisk-token
   :slash-token
   :percent-token
   :plus-plus-token
   :minus-minus-token
   :less-than-less-than-token
   :greater-than-greater-than-token
   :greater-than-greater-than-greater-than-token
   :ampersand-token
   :bar-token
   :caret-token
   :exclamation-token
   :tilde-token
   :ampersand-ampersand-token
   :bar-bar-token
   :question-token
   :colon-token
   :at-token
   ;; Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds.
   :backtick-token
   ;; Assignments
   :equals-token
   :plus-equals-token
   :minus-equals-token
   :asterisk-equals-token
   :asterisk-asterisk-equals-token
   :slash-equals-token
   :percent-equals-token
   :less-than-less-than-equals-token
   :greater-than-greater-than-equals-token
   :greater-than-greater-than-greater-than-equals-token
   :ampersand-equals-token
   :bar-equals-token
   :caret-equals-token
   ;; Identifiers
   :identifier
   ;; Reserved words
   :break-keyword
   :case-keyword
   :catch-keyword
   :class-keyword
   :const-keyword
   :continue-keyword
   :debugger-keyword
   :default-keyword
   :delete-keyword
   :do-keyword
   :else-keyword
   :enum-keyword
   :export-keyword
   :extends-keyword
   :false-keyword
   :finally-keyword
   :for-keyword
   :function-keyword
   :if-keyword
   :import-keyword
   :in-keyword
   :instance-of-keyword
   :new-keyword
   :null-keyword
   :return-keyword
   :super-keyword
   :switch-keyword
   :this-keyword
   :throw-keyword
   :true-keyword
   :try-keyword
   :type-of-keyword
   :var-keyword
   :void-keyword
   :while-keyword
   :with-keyword
   ;; Strict mode reserved words
   :implements-keyword
   :interface-keyword
   :let-keyword
   :package-keyword
   :private-keyword
   :protected-keyword
   :public-keyword
   :static-keyword
   :yield-keyword
   ;; Contextual keywords
   :abstract-keyword
   :as-keyword
   :any-keyword
   :async-keyword
   :await-keyword
   :boolean-keyword
   :constructor-keyword
   :declare-keyword
   :get-keyword
   :infer-keyword
   :is-keyword
   :key-of-keyword
   :module-keyword
   :namespace-keyword
   :never-keyword
   :readonly-keyword
   :require-keyword
   :number-keyword
   :object-keyword
   :set-keyword
   :string-keyword
   :symbol-keyword
   :type-keyword
   :undefined-keyword
   :unique-keyword
   :unknown-keyword
   :from-keyword
   :global-keyword
   :big-int-keyword
   :of-keyword ;; LastKeyword and LastToken and LastContextualKeyword

   ;; Parse tree nodes

   ;; Names
   :qualified-name
   :computed-property-name
   ;; Signature elements
   :type-parameter
   :parameter
   :decorator
   ;; TypeMember
   :property-signature
   :property-declaration
   :method-signature
   :method-declaration
   :constructor
   :get-accessor
   :set-accessor
   :call-signature
   :construct-signature
   :index-signature
   ;; Type
   :type-predicate
   :type-reference
   :function-type
   :constructor-type
   :type-query
   :type-literal
   :array-type
   :tuple-type
   :optional-type
   :rest-type
   :union-type
   :intersection-type
   :conditional-type
   :infer-type
   :parenthesized-type
   :this-type
   :type-operator
   :indexed-access-type
   :mapped-type
   :literal-type
   :import-type
   ;; Binding patterns
   :object-binding-pattern
   :array-binding-pattern
   :binding-element
   ;; Expression
   :array-literal-expression
   :object-literal-expression
   :property-access-expression
   :element-access-expression
   :call-expression
   :new-expression
   :tagged-template-expression
   :type-assertion-expression
   :parenthesized-expression
   :function-expression
   :arrow-function
   :delete-expression
   :type-of-expression
   :void-expression
   :await-expression
   :prefix-unary-expression
   :postfix-unary-expression
   :binary-expression
   :conditional-expression
   :template-expression
   :yield-expression
   :spread-element
   :class-expression
   :omitted-expression
   :expression-with-type-arguments
   :as-expression
   :non-null-expression
   :meta-property
   :synthetic-expression

   ;; Misc
   :template-span
   :semicolon-class-element
   ;; Element
   :block
   :variable-statement
   :empty-statement
   :expression-statement
   :if-statement
   :do-statement
   :while-statement
   :for-statement
   :for-in-statement
   :for-of-statement
   :continue-statement
   :break-statement
   :return-statement
   :with-statement
   :switch-statement
   :labeled-statement
   :throw-statement
   :try-statement
   :debugger-statement
   :variable-declaration
   :variable-declaration-list
   :function-declaration
   :class-declaration
   :interface-declaration
   :type-alias-declaration
   :enum-declaration
   :module-declaration
   :module-block
   :case-block
   :namespace-export-declaration
   :import-equals-declaration
   :import-declaration
   :import-clause
   :namespace-import
   :named-imports
   :import-specifier
   :export-assignment
   :export-declaration
   :named-exports
   :export-specifier
   :missing-declaration

   ;; Module references
   :external-module-reference

   ;; JSX
   :jsx-element
   :jsx-self-closing-element
   :jsx-opening-element
   :jsx-closing-element
   :jsx-fragment
   :jsx-opening-fragment
   :jsx-closing-fragment
   :jsx-attribute
   :jsx-attributes
   :jsx-spread-attribute
   :jsx-expression

   ;; Clauses
   :case-clause
   :default-clause
   :heritage-clause
   :catch-clause

   ;; Property assignments
   :property-assignment
   :shorthand-property-assignment
   :spread-assignment

   ;; Enum
   :enum-member
   ;; Unparsed
   :unparsed-prologue
   :unparsed-prepend
   :unparsed-text
   :unparsed-internal-text
   :unparsed-synthetic-reference

   ;; Top-level nodes
   :source-file
   :bundle
   :unparsed-source
   :input-files

   ;; JSDoc nodes
   :js-doc-type-expression
   ;; The * type
   :js-doc-all-type
   ;; The ? type
   :js-doc-unknown-type
   :js-doc-nullable-type
   :js-doc-non-nullable-type
   :js-doc-optional-type
   :js-doc-function-type
   :js-doc-variadic-type
   :js-doc-comment
   :js-doc-type-literal
   :js-doc-signature
   :js-doc-tag
   :js-doc-augments-tag
   :js-doc-class-tag
   :js-doc-callback-tag
   :js-doc-enum-tag
   :js-doc-parameter-tag
   :js-doc-return-tag
   :js-doc-this-tag
   :js-doc-type-tag
   :js-doc-template-tag
   :js-doc-typedef-tag
   :js-doc-property-tag

   ;; Synthesized list
   :syntax-list

   ;; Transformation nodes
   :not-emitted-statement
   :partially-emitted-expression
   :comma-list-expression
   :merge-declaration-marker
   :end-of-declaration-marker

   ;; Enum value count
   :count))

(in-package :esinco.syntaxkind)

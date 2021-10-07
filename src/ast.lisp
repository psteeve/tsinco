(defpackage :tsinco.ast
  (:use
   :common-lisp
   :st-utils))

(in-package :tsinco.ast)



































(defclass variable-statement (statement)
  ((kind :initarg :kind :initform :variable-statement :reader kind)
   (declaration-list :initarg :declatarion-list :initform (error "Must supply value for declaration-list.") :reader declaration-list)))

(defclass variable-declaration (named-declaration)
  ((kind :initarg :kind :initform :variable-declaration :reader kind)
   (parent :initarg :parent :initform (error "Must supply value for parent") :reader parent)
   (name :initarg :name :initform (error "Must supply a value for name") :reader name)
   (initializer :initarg :initializer :initform (error "Must supply a value form initializer.") :reader initializer)
   (exclamation-token :initarg :exclamation-token :reader exclamation-token)
   (v-type :initarg :v-type :reader v-type)))

(defclass interface-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :interface-declaration
         :reader kind)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)
   (type-parameters :initarg :type-parameters
                    :reader type-parameters)
   (heritage-clauses :initarg :heritage-clauses
                     :reader heritage-clauses)
   (members :initarg :members
            :initform (error "Must supply a value for \"members\"")
            :reader members)))

(defclass variable-declaration-list (node)
  ((kind :initarg :kind :initform :variable-declaration-list :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent") :reader parent)
   (declarations :initarg declarations :initform (error "Must supply a value for declarations.") :reader declarations)))

(defclass iteration-statement (statement)
  ((statement :initarg :statement :initform (error "Must supply a value for statement.") :reader statement)))

(defclass for-of-statement (iteration-statement)
  ((kind :initarg :kind :initform :for-of-statement :reader kind)
   (await-modifier :initarg await-modifier :reader await-modifier)
   (initializer :initarg :for-initializer :initform (error "Must supply a value for initializer.") :reader initializer)
   (expression :initarg :expression :initform (error "Must supply a value for expresson.") :reader expression)))

(defclass for-in-statement (iteration-statement)
  ((kind :initarg :kind :initform :for-in-statement :reader kind)
   (initializer :initarg :for-initializer :initform (error "Must supply a value for initializer.") :reader initializer)
   (expression :initarg :expression :initform (error "Must supply a value for expresson.") :reader expression)))

(defclass for-statement (iteration-statement)
  ((kind :initarg :kind :initform :for-statement :reader kind)
   (initializer :initarg :initializer  :reader initializer)
   (the-condition :initarg :the-condition :reader the-condition)
   (incrementor :initarg :incrementor :reader incrementor)))

(defclass cach-clause (node)
  ((kind :initarg :kind :initform :cach-clause :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)
   (variable-declaration :initarg :variable-declaration :reader variable-declaration)
   (a-block :initarg :a-block :initform (error "Must supply a value for a-block.") :reader a-block)))

(defclass object-binding-pattern (node)
  ((kind :initarg :kind :initform :object-binding-pattern :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value form parent.") :reader parent)
   (elements :initarg :elements :initform (error "Must supply a value form elements.") :reader elements)))

(defclass binding-element (named-declaration)
  ((kind :initarg :kind :initform :binding-element :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)
   property-name
   dotdot-token
   (name :initarg :name :initform (error "Must supply a value for name") :reader name)
   initializer))

(defclass array-binding-pattern (node)
  ((kind :initarg :kind :initform :array-binding-pattern :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)
   (elements :initarg :elements :initform (error "Must supply a value for elements.") :reader elements)))

(defclass type-alias-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :type-alias-declaration
         :reader kind)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)
   type-parameters
   (alias-type :initarg :type
               :initform (error "Must supply a value for \"type\"")
               :reader alias-type)))

(defclass type-element (named-declaration)
  (name
   question-token))

(defclass property-signature (type-element)
  ((kind :initarg :kind :initform :property-signature :reader kind)
   (name :initarg :name :initform (error "Must supply a value for name.") :reader name)
   (question-token :initarg :question-token :reader question-token)
   (initializer :initarg :initializer :reader initializer)))

(defclass void-expression (unary-expression)
  ((kind :initarg :kind :initform :void-expression :reader kind)
   (expression :initarg :expression :initform (error "Must supply a value for expression.") :reader expression)))

(defclass token (node)
  ((kind :initarg :kind :initform :token :reader kind)))

(defun make-token (kind pos end)
  (make-instance 'token :kind kind
                        :pos pos
                        :end end))

(defclass template-expression (primary-expression)
  ((kind :initarg :kind :initform :template-expression :reader kind)
   (head :initarg :head :initform (error "Must suplly a value for head.") :reader head)
   (template-sans :initarg :template-sans :initform (error "Must supply a value for template-sans.") :reader template-sans)))

(defclass template-head (literal-like-node)
  ((kind :initarg :template-head :initform :template-head :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value form parent.") :reader parent)))

(defclass template-tail (literal-like-node)
  ((kind :initarg :kind :initform :template-head :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)))

(defclass template-span (node)
  ((kind :initarg :kind :initform :template-span :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)   
   (expression :initarg :expression :initform (error "Must supply a value for expression.") :reader expression)
   (literal :initarg :literal :initform (error "Must supply a value for literal.") :reader literal)))

(defclass template-middle (literal-like-node)
  ((kind :initarg :kind :initform :template-middle :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent.") :reader parent)))

(defclass array-literal-expression (primary-expression)
  ((kind :initarg :kind :initform :array-literal-expression :reader kind)
   (elements :initarg :elements :initform (error "Must supply a value for elements.") :reader elements)
   (multi-line :initarg :multi-line :reader multi-line?)))

(defclass parenthesized-expression (primary-expression)
  ((kind :initarg :kind :initform :parenthesized-expression :reader kind)
   (expression :initarg :expression :initform (error "Must supply a value for expression.") :reader expression)))

(defclass project-reference ()
  ((path :initarg path :reader path)
   (original-pathp :initarg :original-path-p :reader original-path-p)
   (prependp :initarg :prependp :reader prependp)
   (circularp :initarg :circularp :reader circularp)))

(defclass class-like-declaration-base (named-declaration)
  ((kind :initarg :kind
         :initform :class-like-declaration-base
         :reader kind)
   (name :initarg :name
         :reader name)
   (type-parameters :initarg :type-parameters
                    :reader type-parameters)
   (heritage-clauses :initarg :heritage-clauses
                     :reader heritage-clauses)
   (members :initarg :members
            :initform (error "Must supply a value for \"members\"")
            :reader members)))

(defclass object-literal-element (named-declaration)
  ((object-literal-brand :initarg :object-literal-brand
                         :reader object-literal-brand)
   (name :initarg :name
         :reader name)))

(defclass object-literal-expression-base (object-literal-element primary-expression named-declaration)
  ((properties :initarg :properties
               :initform (error "Must supply a value for \"properties\"")
               :reader properties)))

(defmethod initialize-instance :after ((object object-literal-expression-base) &key)
  (with-slots (properties) object
    (setf properties (mapcar #'parse-simple-sexp properties))))

(defclass object-literal-expression (object-literal-expression-base)
  ((multi-line :initarg :multi-line :reader multi-line)))


(defclass script-reference-host () ())

(defclass node-with-type-arguments (type-node)
  ((type-arguments :initarg :type-arguments
                   :reader type-arguments)))


(defgeneric get-compiler-options (script-reference-host))
(defgeneric get-source-file (script-reference-host file-name))
(defgeneric get-source-file-by-path (script-reference-host path))
(defgeneric get-current-directory (script-reference-host))

(defgeneric get-root-file-names (program)
  (:documentation "Get a list of root file names that where passed to a \"createProgram\""))

(defgeneric get-source-files (program)
  (:documentation "Get a list of files in the program."))

(defgeneric emit (program &key
                            target-source-file?
                            write-file?
                            cancellation-token?
                            emit-only-dts-files?
                            custom-transformers?
                            ))

(defgeneric get-options-diagnostics (program cancellation-token?))
(defgeneric get-global-diagnostics (program cancellation-token?))
(defgeneric get-syntactic-diagnostics (program &key
                                                 source-file?
                                                 cancellation-token?))

(defgeneric get-semantic-diagnostics (program &key
                                                source-file?
                                                cancellation-token?))

(defgeneric get-declaration-diagnostics (program &key
                                                   source-file?
                                                   cancellation-token?))

(defgeneric get-config-file-parsing-diagnostics (program))

(defgeneric get-type-checker (program))

(defgeneric is-source-file-from-external-library (program))
(defgeneric is-source-file-from-default-library (program))
(defgeneric get-project-references (program))
(defgeneric get-resolved-project-references (program))

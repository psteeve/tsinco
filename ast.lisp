(defpackage :tsinco.ast
  (:use
   :common-lisp
   :st-utils))

(in-package :tsinco.ast)

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (let ((car-of-form (car form)))
         (or (funcall test car-of-form)
             (and (cons-form-p car-of-form))))))

(defun make-instance-for (object-name value)
  (apply #'make-instance object-name value))

(defun parse-simple-sexp (sexp)
  "Parse an expression of the form (:tag :prop 1 :prop2 2 :prop3 3)
to produce an object of type tag
example: (parse-simple-sexp '(:identifier :pos 20 :end 34 :escaped-text \"log\"))
return an object identifier with props (:pos 20 :end 34 :escaped-text \"log\")
assume the definition of identifier exists."
  (destructuring-bind (tag &rest props) sexp
    (let ((object-name (find-symbol (symbol-name tag))))
      (make-instance-for object-name props))))

(defclass text-range ()
  ((pos :initarg :pos :initform 0 :reader pos)
   (end :initarg :end
        :initform (error "Must supply a value for \"end\"")
        :reader end)))

(defclass node (text-range)
  ((kind :initarg :kind :initform :node :accessor kind) 
   (flags :initarg :flags :reader flags)
   (decorators :initarg :decorators :accessor decorators)
   (modifiers :initarg :modifiers :accessor modifiers)
   (id :initarg :id :accessor id)
   (parent :initarg :parent :accessor parent)
   (modifier-flags-cache :initarg :modifier-flags-cache :accessor modifier-flags-cache)
   (transform-flags :initarg :transform-flags :accessor transform-flags)))

(defclass decorator (node)
  ((kind :initarg :kind :initform :decorator :reader kind)
   (expression :initarg :expression
               :accessor expression)))

(defmethod initialize-instance :after ((object decorator) &key)
  (with-slots (expression) object
    (setf expression (parse-simple-sexp expression))))

(defclass type-node (node)
  ((type-node-brand :initarg :type-node-brand
                    :reader type-node-brand)))

(defclass keyword-type-node (type-node) ())

(defmacro define-keyword-type-node (types)
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(defclass ,x (keyword-type-node)
                      ((kind :initarg :kind
                             :initform ,(st-utils:as-keyword x)
                             :reader kind))))
               types)))

(define-keyword-type-node (any-keyword
                           unknown-keyword
                           number-keyword
                           big-int-keyword
                           object-keyword
                           boolean-keyword
                           string-keyword
                           symbol-keyword
                           this-keyword
                           void-keyword
                           undefined-keyword
                           null-keyword
                           never-keyword))

(defclass class-declaration (class-like-declaration-base declaration-statement)
  ((kind :initarg :kind
         :initform :class-declaration
         :reader kind)
   (name :initarg :name
         :reader name)))

(defmethod initialize-instance :after ((object class-declaration) &key)
  (with-slots (decorators name modifiers) object
    (setf decorators (mapcar #'parse-simple-sexp (decorators object)))
    (setf name (parse-simple-sexp (name object)))
    (setf modifiers (mapcar #'parse-simple-sexp (modifiers object)))))

(defclass heritage-clause (node)
  ((kind :initarg :kind
         :initform :heritage-clause
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (token :initarg :token
          :initform (error "Must supply a value for \"token\"")
          :reader token)
   (types :initarg :types
          :initform (error "Must supply a value for \"types\"")
          :reader types)))

(defclass case-block (node)
  ((kind :initarg :kind
         :initform :case-block
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must provide a value for \"parent\"")
           :reader parent)
   (clauses :initarg :clauses
            :initform (error "Must provide a value for \"clauses\"")
            :reader clauses)))

(defclass case-clause (node)
  ((kind :initarg :kind
         :initform :case-clause
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must provide a value for \"parent\"")
           :reader parent)
   (expression :initarg :expression
               :initform (error "Must provide a value for \"expression\"")
               :reader expression)
   (statements :initarg :statements
               :initform (error "Must provide a value for \"statements\"")
               :reader statements
               )))

(defclass default-clause (node)
  ((kind :initarg :kind
         :initform :default-clause
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must provide a value for \"parent\"")
           :reader parent)
   (statements :initarg :statements
               :initform (error "Must provide a value for \"statements\"")
               :reader statements)))

(defclass switch-statement (statement)
  ((kind :initarg :kind :initform :switch-statement :reader kind)
   (expression :initarg :expression :initform (error "Must provide a value for \"expression\"") :reader expression)
   (case-block :initarg :case-block :initform (error "Must provide a value for \"case-block\"") :reader case-block)
   (possibly-exhaustive :initarg :possibly-exhaustive :reader possibly-exhaustive)))

(defclass a-declaration (node) ())

(defclass primary-expression (member-expression)
  ())

(defclass member-expression (left-hand-side-expression)
  ())

(defclass left-hand-side-expression (update-expression)
  ())

(defclass update-expression (unary-expression)
  ())

(defclass unary-expression (expression) ())

(defclass identifier (primary-expression a-declaration)
  ((kind :initarg :kind :initform :identifier)
   (escaped-text :initarg :escaped-text
                 :reader :escaped-text)))

(defclass expression (node)
  ((kind :initarg :kind :initform :expression)))

(defclass statement (node) ())

(defclass expression-statement (statement)
  ((kind :initarg :kind :initform :expression-statement :reader kind)
   (expression :initarg :expression :initform (error "Must supply a value for expression") :reader expression)))

(defmethod initialize-instance :after ((object expression-statement) &key)
  (with-slots (expression) object
    (setf expression (parse-simple-sexp (expression object)))))

(defclass node-array (node text-range)
  ((has-trailing-comma :initarg :has-trailing-comma :reader has-trailing-comma?)
   (node-list :initarg :node-list :initform (error "Must supply a value node-list") :reader node-list)))

(defmethod initialize-instance :after ((object node-array) &key)
  (with-slots (node-list) object
    (setf node-list (mapcar #'parse-simple-sexp (node-list object)))))

(defclass method-signature (signature-declaration-base type-element)
  ((kind :initarg :kind
         :initform (error "Must supply a value for \"kind\"")
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)))

(defclass call-expression (left-hand-side-expression a-declaration) 
  ((kind :initarg :kind :initform :call-expression :reader kind)
   (expression :initarg :expression :initform (error "Must suplly a value for expression") :reader expression)
   (type-arguments :initarg :type-arguments :accessor type-arguments)
   (arguments :initarg :arguments :initform (error "Must supply a value for arguments") :reader arguments)))

(defmethod initialize-instance :after ((object call-expression) &key)
  (with-slots (expression arguments) object
    (setf expression (parse-simple-sexp (expression object)))
    (setf arguments (parse-simple-sexp (arguments object)))))

(defclass string-literal (literal-expression)
  ((kind :initarg :kind :initform :string-literal :reader kind)))

(defclass literal-expression (literal-like-node primary-expression) ())

(defclass literal-like-node (node)
  ((text :initarg :text :initform (error "Must supply a value") :reader text)
   (is-unterminated :initarg :is-untermnated :reader is-unterminated?)
   (has-extended-unicode-escape :initarg :has-extended-unicode-escape :reader has-extended-unicode-escape?)))

(defclass program (script-reference-host) ())

(defclass return-statement (statement)
  ((kind :initarg :kind :initform :return-statement :reader kind)
   (expression :initarg :expression :reader expression)))

(defmethod initialize-instance :after ((object return-statement) &key)
  (with-slots (expression) object
    (if expression
        (setf expression (parse-simple-sexp (expression object))))))

(defclass token (node)
  ((kind :initarg :kind :initform (error "Must supply a value for \"kind\"")
         :reader kind)))

(defclass plus-token (token)
  ((kind :initarg :kind :initform :plus-token)))

(defclass binary-expression (expression a-declaration)
  ((kind :initarg :kind :initform :binary-expression :reader kind)
   (left :initarg :left :initform (error "Must supply a value for left") :reader left)
   (operator-token :initarg :operator-token
                   :initform (error "Must suppla a value for \"operator-token\"")
                   :reader operator-token)
   (right :initarg :right :initform (error "Must supply a value for right") :reader right)))

(defmethod initialize-instance :after ((object binary-expression) &key)
  (with-slots (left right operator-token) object
    (setf left (parse-simple-sexp (left object)))
    (setf operator-token (parse-simple-sexp (operator-token object)))
    (setf right (parse-simple-sexp (right object)))))

(defclass numeric-literal (literal-expression)
  ((kind :initarg :kind :initform :numeric-literal :reader kind)
   (numeric-literal-flags :initarg :numeric-literal-flags :reader numeric-literal-flags)))

(defclass enum-member (named-declaration)
  ((kind :initarg :kind :initform :enum-member :reader kind)
   (name :initarg :name :initform (error "Must supply a value for \"name\"") :reader name)
   (initializer :initarg :initializer :reader initializer)))

(defclass enum-declaration (declaration-statement)
  ((kind :initarg :kind :initform :enum-declaration :reader kind)
   (name :initarg :name :initform (error "Must supply a value for \"name\"") :reader name)
   (members :initarg :members :initform (error "Must supply a value for \"members\"") :reader members)))

(defclass property-access-expression (member-expression named-declaration)
  ((kind :initarg :kind :initform :property-access-expression :reader kind)
   (expression :initarg :expression :initform (error "Must supply an expression value") :reader expression)
   (name :initarg :name :initform (error "Must supplly a name value") :reader name)))

(defmethod initialize-instance :after ((object property-access-expression) &key)
  (with-slots (name expression) object
    (setf name (parse-simple-sexp (name object)))
    (setf expression (parse-simple-sexp (expression object)))))

(defclass named-declaration (a-declaration) ())

(defclass parameter (named-declaration)
  ((kind :initarg :kind :initform :parameter :reader kind)
   (parent :initarg :parent :reader parent)
   (name :initarg :name :initform (error "Must supply value for name") :reader name)
   exclamation-token
   (type-parameter :initarg :type-parameter :reader type-parameter)
   initializer))

(defmethod initialize-instance :after ((object parameter) &key)
  (with-slots (name type-parameter) object
    (setf name (parse-simple-sexp (name object)))
    (setf type-parameter (parse-simple-sexp (type-parameter object)))))

(defclass if-statement (statement)
  ((kind :initarg :kind :initform :if-statement :reader kind)
   (expression :initarg :expression :initform (error "Must supply value for expression") :reader expression)
   (then-statement :initarg :then-statement :initform (error "Must supply value for then-statement") :reader then-statement)
   (else-statement :initarg :else-statement :reader else-statement)))

(defclass signature-declaration-base (named-declaration)
  ((kind :initarg :kind :initform :signature-declaration-base :reader kind)
   (name :initarg :name :reader name)
   (type-parameters :initarg :type-parameters :initform nil :reader type-parameters)
   (parameters :initarg :parameters :initform (error "Must supply a value for \"parameters\"") :reader parameters)
   (sig-type :initarg :sig-type :reader sig-type)))

(defclass declaration-statement (named-declaration statement)
  ((name :initarg :name :reader name)))

(defclass type-parameter-declaration (named-declaration)
  ((kind :initarg :kind
         :initform :type-parameter-declaration
         :reader type-parameter-declaration)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)
   constraint
   default
   expression))

(defclass contruct-signature-declaration (signature-declaration-base)
  ((kind :initarg :kind :initarg :construct-signature-declaration :reader kind)))

(defclass function-like-declaration-base (signature-declaration-base)
  (asterisk-token
   question-token
   exclamation-token
   body))

(defclass function-declaration (function-like-declaration-base declaration-statement)
  ((kind :initarg :kind :initform :function-declaration :reader function-declaration)
   (name :initarg :name :reader name)
   (body :initarg :body :reader body)))

(defmethod initialize-instance :after ((object function-declaration) &key)
  (with-slots (name body parameters modifiers) object
    (setf parameters (mapcar #'parse-simple-sexp (parameters object)))
    (setf name (parse-simple-sexp (name object)))
    (setf body (parse-simple-sexp (body object)))
    (setf modifiers (mapcar #'parse-simple-sexp (modifiers object)))))

(defclass a-block (statement)
  ((kind :initarg :kind :initform :a-block :reader kind)
   (multi-line :initarg :multi-line :reader multi-line)
   (statements :initarg :statements
               :initform (error "Must supply value for statements")
               :reader statements)))

(defmethod initialize-instance :after ((object a-block) &key)
  (with-slots (statements) object
    (setf statements (mapcar #'parse-simple-sexp (statements object)))))

(defclass token (node)
  ((kind :initarg :kind :initform :token :reader kind)))

(defmacro define-token-class (slots)
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(defclass ,x (token)
                      ((kind :initarg :kind
                             :initform ,(st-utils:as-keyword x)
                             :reader kind))))
               slots)))


(defclass module-block (statement)
  ((kind :initarg :kind
         :initform :module-block
         :reader kind)
   (statements :initarg :statements
               :reader statements
               :initform (error "Must supply a value for \"statements\""))))

(defmethod initialize-instance :after ((object module-block) &key)
  (with-slots (statements) object
    (setf statements (mapcar #'parse-simple-sexp (statements object)))))

(defclass module-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :module-declaration
         :reader kind)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)
   (body :initarg :body
         :reader body)))

(defmethod initialize-instance :after ((object module-declaration) &key)
  (with-slots (body name) object
    ;;    (setf body (mapcar #'parse-simple-sexp (body object)))
    (setf name (mapcar #'parse-simple-sexp (name object)))))

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

(defclass property-assignment (object-literal-element)
  ((parent :initarg :parent :reader parent)
   (kind :initarg :property-assignment :reader kind)
   (name :initarg :name :reader name)
   (question-token-? :initarg :question-token-? :reader question-token-?)
   (initializer :initarg :initializer :reader initializer)))

(defmethod initialize-instance :after ((object property-assignment) &key)
  (with-slots (name initializer) object
    (setf name (parse-simple-sexp name))
    (setf initializer (parse-simple-sexp initializer))))


(defclass script-reference-host () ())

(defclass import-clause (named-declaration)
  ((kind :initarg :kind
         :initform :import-clause
         :reader kind)
   (parent :initarg :parent
           :reader parent)
   (name :initarg :name
         :reader name)
   (is-type-only :initarg :is-type-only :reader is-type-only)
   (named-bindings :initarg :named-bindings
                   :reader named-bindings)))

(defmethod initialize-instance :after ((object import-clause) &key)
  (with-slots (named-bindings) object
    (setf named-bindings (parse-simple-sexp (named-bindings object)))))

(defclass import-specifier (named-declaration)
  ((kind :initarg :kind
         :initform :import-specifier
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (property-name :initarg :property-name
                  :reader property-name)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\"")
         :reader name)))

(defclass named-imports (node)
  ((kind :initarg :kind
         :initform :named-imports
         :reader kind)
   (parent :initarg :parent
           :reader parent)
   (elements :initarg :elements
             :initform (error "Must supply a value for \"elements\""))))

(defclass export-speficier (named-declaration)
  ((kind :initarg :kind
         :initform :export-specifier
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (property-name :initarg :property-name
                  :reader property-name)
   (name :initarg :name
         :initform (error "Must supply a value for \"initform\"")
         :reader name)))

(defclass import-declaration (statement)
  ((kind :initarg :kind
         :initform :import-declaration
         :reader kind)
   (import-clause :initarg :import-clause
                  :reader import-clause)
   (module-specifier :initarg :module-specifier
                     :initform (error "Must supply a value for \"module-specifier\"")
                     :reader module-specifier)))

(defmethod initialize-instance :after ((object import-declaration) &key)
  (with-slots (import-clause module-specifier) object
    (setf import-clause (parse-simple-sexp (import-clause object)))
    (setf module-specifier (parse-simple-sexp (module-specifier object)))))

(defclass export-declaration (declaration-statement)
  ((kind :initarg :kind
         :initform :export-declaration
         :reader kind)
   (parent :initarg :parent
           :initform (error "Must supply a value for \"parent\"")
           :reader parent)
   (import-clause :initarg :import-clause
                  :reader import-clause)
   (module-specifier :initarg :module-specifier
                     :reader module-specifier)))

(defclass namespace-import (named-declaration)
  ((kind :initarg :kind
         :initform :namespace-import
         :reader kind)
   (parent :initarg :parent :reader parent)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\""))))

(defclass node-with-type-arguments (type-node)
  ((type-arguments :initarg :type-arguments
                   :reader type-arguments)))

(defclass type-reference (node-with-type-arguments)
  ((kind :initarg :kind
         :initform :type-reference
         :reader kind)
   (type-name :initarg :type-name
              :initform (error "Must supply a value for \"type-name\"")
              :reader type-name)))

(defmethod initialize-instance :after ((object type-reference) &key)
  (with-slots (type-name type-arguments) object
    (setf type-name (parse-simple-sexp (type-name object)))))

(defclass qualified-name (node)
  ((kind :initarg :kind
         :initform :qualified-name
         :reader kind)
   (left :initarg :left
         :initform (error "Must supply a value for \"left\"")
         :reader left)
   (right :initarg :right
          :initform (error "Must supply a value for \"right\"")
          :reader right)))

(defclass new-expression (primary-expression declaration)
  ((kind :initarg :kind
         :initform :new-expression
         :reader kind)
   (expression :initarg :expression
               :initform (error "Must supply a value for \"expression\"")
               :reader expression)
   (type-arguments :initarg :type-arguments
                   :reader type-arguments)
   (arguments :initarg :arguments
              :reader arguments)))

(defclass source-file (a-declaration)
  ((kind :initarg :kind
         :initform :source-file
         :reader kind)
   (statements :initarg :statements
               :initform (error "Must supply a value for statements")
               :reader statements)
   (end-of-file-token :initarg :end-of-file-token
                      :reader end-of-file-token)
   (filename :initarg :filename
             :initform (error "Must supply a filename")
             :reader filename)
   (text :initarg :text
         :initform (error "Must supply a value for text")
         :reader text)
   (external-module-indicator :initarg :external-module-indicator
                              :initform nil
                              :accessor external-module-indicator)
   (amd-dependencies :initarg :amd-dependencies
                     :initform nil
                     :accessor amd-dependencies)
   (module-name :initarg :module-name
                :accessor module-name)
   (referenced-files :initarg :referenced-files
                     :initform nil
                     :accessor referenced-files)
   (type-reference-directives :initarg :type-reference-directives
                              :initform nil
                              :accessor type-reference-directives)
   (lib-reference-directives :initarg :lib-reference-directives
                             :initform nil
                             :accessor lib-reference-directives)
   (language-variant :initarg :language-variant
                     :initform :standard
                     :reader language-variant)
   (is-declaration-file :initarg :is-declaration-file
                        :accessor is-declaration-file)
   (has-no-default-lib? :initarg :has-no-default-lib
                        :initform nil
                        :reader has-no-default-lib?)
   (language-version :initarg :language-version
                     :initform :es5
                     :reader language-version)
   (parse-diagnostics :initarg :parse-diagnostics
                      :reader parse-diagnostics)
   
   (node-count :initarg :node-count
               :reader node-count)
   (pragmas :initarg :pragmas
            :reader pragmas)
   (bind-diagnostics :initarg :bind-diagnostics
                     :reader bind-diagnostics)
   (identifiers :initarg :identifiers
                :reader identifiers)
   (identifier-count :initarg :identifier-count
                     :reader identifier-count)
   (script-kind :initarg :script-kind
                :reader script-kind)
   (transform-flags :initarg :transform-flags
                    :reader transform-flags)
   (ambient-module-names :initarg :ambient-module-names
                         :reader ambient-module-names)
   (module-augmentations :initarg :module-augmentations
                         :reader module-augmentations)
   (imports :initarg :imports
            :reader imports)
   (original-file-name :initarg :original-file-name
                       :reader original-file-name)
   (resolved-path :initarg :resolved-path
                  :reader resolved-path)
   (path :initarg :path
         :reader path)
   (modifier-flags-cache :initarg :modifier-flags-cache
                         :reader modifier-flags-cache)))


(defmethod initialize-instance :after ((object source-file) &key)
  (with-slots (statements end-of-file-token) object
    (setf statements (mapcar #'parse-simple-sexp (statements object)))
    (setf end-of-file-token (parse-simple-sexp (end-of-file-token object)))))

(defclass program (script-reference-host) ())

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

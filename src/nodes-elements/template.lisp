
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


(defclass template-expression (primary-expression)
  ((kind :initarg :kind :initform :template-expression :reader kind)
   (head :initarg :head :initform (error "Must suplly a value for head.") :reader head)
   (template-sans :initarg :template-sans :initform (error "Must supply a value for template-sans.") :reader template-sans)))

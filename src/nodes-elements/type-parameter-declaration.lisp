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

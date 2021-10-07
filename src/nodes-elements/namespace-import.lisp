(defclass namespace-import (named-declaration)
  ((kind :initarg :kind
         :initform :namespace-import
         :reader kind)
   (parent :initarg :parent :reader parent)
   (name :initarg :name
         :initform (error "Must supply a value for \"name\""))))

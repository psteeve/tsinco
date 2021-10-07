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

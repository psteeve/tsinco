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

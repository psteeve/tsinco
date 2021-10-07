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


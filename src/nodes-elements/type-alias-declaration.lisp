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

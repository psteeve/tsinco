(defclass variable-declaration-list (node)
  ((kind :initarg :kind :initform :variable-declaration-list :reader kind)
   (parent :initarg :parent :initform (error "Must supply a value for parent") :reader parent)
   (declarations :initarg declarations :initform (error "Must supply a value for declarations.") :reader declarations)))

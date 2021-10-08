(defclass variable-statement (statement)
  ((kind :initarg :kind :initform :variable-statement :reader kind)
   (declaration-list :initarg :declatarion-list
                     :initform (error "Must supply value for declaration-list.")
                     :reader declaration-list)))

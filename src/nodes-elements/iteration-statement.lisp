(defclass iteration-statement (statement)
  ((statement :initarg :statement :initform (error "Must supply a value for statement.") :reader statement)))


(defclass for-in-statement (iteration-statement)
  ((kind :initarg :kind :initform :for-in-statement :reader kind)
   (initializer :initarg :for-initializer :initform (error "Must supply a value for initializer.") :reader initializer)
   (expression :initarg :expression :initform (error "Must supply a value for expresson.") :reader expression)))

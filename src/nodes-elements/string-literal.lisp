(defclass string-literal (literal-expression)
  ((kind :initarg :kind :initform :string-literal :reader kind)))

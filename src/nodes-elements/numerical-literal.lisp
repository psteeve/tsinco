(defclass numeric-literal (literal-expression)
  ((kind :initarg :kind :initform :numeric-literal :reader kind)
   (numeric-literal-flags :initarg :numeric-literal-flags :reader numeric-literal-flags)))

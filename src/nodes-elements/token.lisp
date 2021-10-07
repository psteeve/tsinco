(defclass token (node)
  ((kind :initarg :kind :initform (error "Must supply a value for \"kind\"")
         :reader kind)))

(defmacro define-token-class (slots)
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(defclass ,x (token)
                      ((kind :initarg :kind
                             :initform ,(st-utils:as-keyword x)
                             :reader kind))))
               slots)))

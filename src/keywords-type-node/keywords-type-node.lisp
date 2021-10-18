
(in-package :tsinco.keywords-type-node)


(defmacro define-keyword-type-node (types)
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(defclass ,x (keyword-type-node)
                      ((kind :initarg :kind
                             :initform ,(st-utils:as-keyword x)
                             :reader kind))))
               types)))


(define-keyword-type-node (any-keyword
                           unknown-keyword
                           number-keyword
                           big-int-keyword
                           object-keyword
                           boolean-keyword
                           string-keyword
                           symbol-keyword
                           this-keyword
                           void-keyword
                           undefined-keyword
                           null-keyword
                           never-keyword))

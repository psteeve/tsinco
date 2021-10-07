(defpackage :tsinco.utils
  (:use :common-lisp)
  (:export
   :cons-form-p))

(in-package :tsinco.utils)

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (let ((car-of-form (car form)))
         (or (funcall test car-of-form)
             (and (cons-form-p car-of-form))))))

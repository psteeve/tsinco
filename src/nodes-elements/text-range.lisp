(in-package :text-range)

(defclass text-range ()
  ((pos :initarg :pos :initform 0 :reader pos)
   (end :initarg :end
        :initform 0
        :reader end)))

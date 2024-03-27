
(in-package :cl-onnx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out))))))

(defmacro range (from to &optional (by 1))
  (alexandria:with-gensyms (a b)
    `(multiple-value-bind (,a ,b) (values (min ,from ,to) (max ,from ,to))
       (loop for i upfrom ,a below ,b by ,by collect i))))


;;; Some IO convenience functions

(in-package :trans-logic.io)

(defvar fmt-out t)
(defvar err-out *error-output*)

(defmacro puts (fmtstr &rest vars)
  (let ((ctl-str (str-cat fmtstr "~%")))
    `(progn
       (format trans-logic.io:fmt-out ,ctl-str ,@vars)
       )
    )
  )

(defmacro e-puts (fmtstr &rest vars)
  (let ((ctl-str (str-cat fmtstr "~%")))
    `(progn
       (format trans-logic.io:err-out ,ctl-str ,@vars)
       )
    )
  )

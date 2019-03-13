(defvar tl-flavor)
(defvar tl-quicklisp)
(cond
  ((equal (cadr sb-ext:*posix-argv*) "smartos")
   (setf tl-flavor "smartos")
   (setf tl-quicklisp "/opt/quicklisp/setup.lisp")
   )
  ((equal (cadr sb-ext:*posix-argv*) "lx")
   (setf tl-flavor "lx")
   (setf tl-quicklisp "~/quicklisp/setup.lisp")
   )
  ((and t)
   (setf tl-flavor "smartos")
   (setf tl-quicklisp "/opt/quicklisp/setup.lisp")
   )
  )

(cond
  ((equal (caddr sb-ext:*posix-argv*) "debug")
   (sb-ext:restrict-compiler-policy 'debug 3)
   )
  )



(load tl-quicklisp)
(asdf:load-asd (concatenate 'string (sb-posix:getcwd) "/trans-logic.asd"))

(ql:quickload :trans-logic)
(ql:quickload :trans-logic-test)

(defun run-tests ()
  (trans-logic-test:run-all-tests)
  )

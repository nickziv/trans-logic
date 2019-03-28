(in-package :cl-user)
(defpackage trans-logic-asd
  (:use :cl :asdf))
(in-package :trans-logic-asd)

(defsystem :trans-logic
  :version "0.1"
  :author "Nick Zivkovic"
  :license "MPL v2 with GPL Licensing Exception"
  :depends-on (
               :cl-ppcre
               :iterate
               )
  :components ((:file "package")
               (:file "data-algebra" :depends-on ("package" "control" "string" "io"))
               (:file "string" :depends-on ("package" "control"))
               (:file "control" :depends-on ("package"))
               (:file "io" :depends-on ("package" "string"))
               )
  )

(defsystem :trans-logic-test
  :depends-on ("trans-logic" :test-utils)
  :components (
               (:module "test"
                :components ((:file "test-data-algebra" :depends-on ("package")
                                    )
                             (:file "package")
                             ))
               )
  )

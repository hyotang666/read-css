; vim: ft=lisp et
(in-package :asdf)
(defsystem "read-css.test"
  :version
  "0.5.0"
  :depends-on
  (:jingoh "read-css")
  :components
  ((:file "read-css"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :read-css args)))

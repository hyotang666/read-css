(defpackage :read-css.spec
  (:use :cl :jingoh :read-css)
  (:import-from :read-css #:read-css))
(in-package :read-css.spec)
(setup :read-css)

(requirements-about READ-CSS :doc-type function)

;;;; Description:

#+syntax (READ-CSS &optional (input *standard-input*)) ; => result

;;;; Arguments and Values:

; input := (or boolean stream) otherwise implementation dependent condition.
#?(read-css "not stream designator") :signals condition

; result := list

;;;; Affected By:

;;;; Side-Effects:
; Consuming the INPUT stream contents.

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
;;; Comment.
#?(with-input-from-string (in "/* comment */")
    (read-css in))
=> NIL

; (Unfortunately) nested comment is disabled.
#?(with-input-from-string (in "/* nested /* comment */ is disabled */")
    (read-css in))
:signals end-of-file

;;; Numbers
; Integer
#?(with-input-from-string (in "0")
    (read-css in))
=> (0)
,:test equal

; Signed integer.
#?(with-input-from-string (in "+1 -1")
    (read-css in))
=> (1 -1)
,:test equal

; Float.
#?(with-input-from-string (in "0.5")
    (read-css in))
=> (0.5)
,:test equal

; Signed float.
#?(with-input-from-string (in "+0.5 -0.5")
    (read-css in))
=> (0.5 -0.5)
,:test equal

; Starts with dot.
#?(with-input-from-string (in ".5 +.5 -.5")
    (read-css in))
=> (0.5 0.5 -0.5)
,:test equal

; Exponential.
#?(with-input-from-string (in "0.5e-3 0.5E-3 +0.5e-3 -0.5E-3 .5e3 .5E3")
    (read-css in))
=> (0.5e-3 0.5e-3 0.5e-3 -0.5e-3 .5e3 .5e3)
,:test equal

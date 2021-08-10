(defpackage :read-css.spec
  (:use :cl :jingoh :read-css)
  (:import-from :read-css #:read-css #:consume-a-number
		#:consume-a-name))
(in-package :read-css.spec)
(setup :read-css)

(requirements-about CONSUME-A-NAME :doc-type function :test equal)

;;;; Description:

#+syntax (CONSUME-A-NAME &optional (input *standard-input*)) ; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "name")
    (consume-a-name in))
=> "name"

; With escape.
#?(with-input-from-string (in "foo\\26 bar")
    (consume-a-name in))
=> "foo&bar"
#?(with-input-from-string (in "foo\\000026bar")
    (consume-a-name in))
=> "foo&bar"
#?(with-input-from-string (in "foo\\0000267bar")
    (consume-a-name in))
=> "foo&7bar"

(requirements-about CONSUME-A-NUMBER :doc-type function)

;;;; Description:
; read number.

#+syntax (CONSUME-A-NUMBER &optional (input *standard-input*)) ; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Integer.
#?(with-input-from-string (in "1") (consume-a-number in)) => 1
#?(with-input-from-string (in "+1") (consume-a-number in)) => 1
#?(with-input-from-string (in "-1") (consume-a-number in)) => -1

; Flort.
#?(with-input-from-string (in "1.0") (consume-a-number in)) => 1.0
#?(with-input-from-string (in "+1.0") (consume-a-number in)) => 1.0
#?(with-input-from-string (in "-1.0") (consume-a-number in)) => -1.0

; Start with dot.
#?(with-input-from-string (in ".5") (consume-a-number in)) => 0.5
#?(with-input-from-string (in "+.5") (consume-a-number in)) => 0.5
#?(with-input-from-string (in "-.5") (consume-a-number in)) => -0.5

; Exponential.
; With lower e.
#?(with-input-from-string (in "1.0e5") (consume-a-number in)) => 1.0e5
#?(with-input-from-string (in "1.0e+5") (consume-a-number in)) => 1.0e5
#?(with-input-from-string (in "1.0e-5") (consume-a-number in)) => 1.0e-5
#?(with-input-from-string (in "+1.0e-5") (consume-a-number in)) => 1.0e-5
#?(with-input-from-string (in "-1.0e-5") (consume-a-number in)) => -1.0e-5

; With upper E.
#?(with-input-from-string (in "1.0E5") (consume-a-number in)) => 1.0e5
#?(with-input-from-string (in "1.0E+5") (consume-a-number in)) => 1.0e5
#?(with-input-from-string (in "1.0E-5") (consume-a-number in)) => 1.0e-5
#?(with-input-from-string (in "+1.0E-5") (consume-a-number in)) => 1.0e-5
#?(with-input-from-string (in "-1.0E-5") (consume-a-number in)) => -1.0e-5

; Unlinke CL:READ, next char is remain.
#?(with-input-from-string (in "1 2") (values (read in) (read-char in)))
:values (1 #\2)
#?(with-input-from-string (in "1 2") (values (consume-a-number in) (read-char in)))
:values (1 #\space)

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

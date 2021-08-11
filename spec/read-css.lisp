(defpackage :read-css.spec
  (:use :cl :jingoh :read-css)
  (:import-from :read-css #:read-css #:consume-a-number #:consume-a-numeric-token
		#:consume-a-name #:consume-a-url-token
		#:consume-an-escaped-code-point))
(in-package :read-css.spec)
(setup :read-css)

(requirements-about CONSUME-AN-ESCAPED-CODE-POINT :doc-type function)

;;;; Description:

#+syntax (CONSUME-AN-ESCAPED-CODE-POINT &optional (input *standard-input*))
; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Case hex digits.
#?(with-input-from-string (in "26") (consume-an-escaped-code-point in))
=> #\&
#?(with-input-from-string (in "000026") (consume-an-escaped-code-point in))
=> #\&

; Case nonsense escaping.
#?(with-input-from-string (in "g") (consume-an-escaped-code-point in))
=> #\g

; Case end-of-file.
#?(with-input-from-string (in "") (consume-an-escaped-code-point in))
:signals end-of-file

; Case newlines.
#?(with-input-from-string (in (string #\newline)) (consume-an-escaped-code-point in))
:signals read-css::invalid-escape

; Case #\nul
#?(with-input-from-string (in "0") (consume-an-escaped-code-point in))
=> #.(code-char #xFFFD)

; Case over unicode point.
#?(with-input-from-string (in "FFFFFF") (consume-an-escaped-code-point in))
=> #.(code-char #xFFFD)

; Case surrogate.
#?(with-input-from-string (in "D800") (consume-an-escaped-code-point in))
=> #.(code-char #xFFFD)

; When hex num is less than six and followed space, such space is consumed well.
#?(with-input-from-string (in "26 a") (values (consume-an-escaped-code-point in)
					      (read-char in nil nil)))
:values (#\& #\a)

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

(requirements-about CONSUME-A-URL-TOKEN :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-URL-TOKEN &optional (input *standard-input*)) ; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "https://example.com/images/myImg.jpg)")
    (consume-a-url-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::url-token)
		(equal "https://example.com/images/myImg.jpg"
		       (read-css::url-token-value result))))

#?(with-input-from-string (in "myFont.woff)")
    (consume-a-url-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::url-token)
		(equal "myFont.woff" (read-css::url-token-value result))))

#?(with-input-from-string (in "#IDofSVGpath)")
    (consume-a-url-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::url-token)
		(equal "#IDofSVGpath" (read-css::url-token-value result))))

; Does not handle quoted string.
#?(with-input-from-string (in "\"star.gif\")")
    (consume-a-url-token in))
:signals read-css::css-parse-error
#?(with-input-from-string (in "'star.gif')")
    (consume-a-url-token in))
:signals read-css::css-parse-error

(requirements-about CONSUME-A-NUMERIC-TOKEN :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-NUMERIC-TOKEN &optional (input *standard-input*))
; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Integer
#?(with-input-from-string (in "1") (consume-a-numeric-token in))
=> 1
#?(with-input-from-string (in "+1") (consume-a-numeric-token in))
=> 1
#?(with-input-from-string (in "-1") (consume-a-numeric-token in))
=> -1

; Float.
#?(with-input-from-string (in "1.0") (consume-a-numeric-token in))
=> 1.0
#?(with-input-from-string (in "+1.0") (consume-a-numeric-token in))
=> 1.0
#?(with-input-from-string (in "-1.0") (consume-a-numeric-token in))
=> -1.0

; Exponention.
#?(with-input-from-string (in "1.0e5") (consume-a-numeric-token in))
=> 1.0e5
#?(with-input-from-string (in "+1.0e+5") (consume-a-numeric-token in))
=> 1.0e+5
#?(with-input-from-string (in "-1.0e-5") (consume-a-numeric-token in))
=> -1.0e-5

; Percentage token.
#?(with-input-from-string (in "50%") (consume-a-numeric-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::percentage-token)
		(eql 50 (read-css::percentage-token-value result))))

; Dimension token.
#?(with-input-from-string (in "10px") (consume-a-numeric-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::dimension-token)
		(eql 10 (read-css::dimension-token-value result))
		(equal "px" (read-css::dimension-token-unit result))))

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
#?(with-input-from-string (in "+1")
    (read-css in))
=> (1)
,:test equal

#?(with-input-from-string (in "-1")
    (read-css in))
=> (-1)
,:test equal

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

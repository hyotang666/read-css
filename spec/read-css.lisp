(defpackage :read-css.spec
  (:use :cl :jingoh :read-css)
  (:import-from :read-css #:read-css #:consume-a-number #:consume-a-numeric-token
		#:consume-a-name #:consume-an-ident-like-token #:consume-a-url-token
		#:consume-an-escaped-code-point #:consume-a-string-token
		#:consume-a-function #:start-an-identifier-p #:consume-a-simple-block
		#:|{-reader| #:|#rgb-reader| #:consume-selectors #:consume-comments))
(in-package :read-css.spec)
(setup :read-css)

(requirements-about CONSUME-COMMENTS :doc-type function)

;;;; Description:

#+syntax (CONSUME-COMMENTS &optional (input *standard-input*)) ; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in " comment */a")
    (values (consume-comments in)
	    (read-char in)))
:values (nil #\a)

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
; Must check VALID-ESCAPE-P before call this.

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

; Case end-of-file without before checking.
#?(with-input-from-string (in "") (consume-an-escaped-code-point in))
:signals read-css::internal-logical-error

; Case newlines without before cheking.
#?(with-input-from-string (in (string #\newline)) (consume-an-escaped-code-point in))
:signals read-css::internal-logical-error

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

(requirements-about START-AN-IDENTIFIER-P :doc-type function)

;;;; Description:

#+syntax (START-AN-IDENTIFIER-P input) ; => result

;;;; Arguments and Values:

; input := css-input-stream

; result := boolean

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "--hoge")
    (let ((in (read-css::ensure-input-stream in)))
      (values (start-an-identifier-p in)
	      (read-char in))))
:values (t #\-)

#?(with-input-from-string (in "-\\hoge")
    (let ((in (read-css::ensure-input-stream in)))
      (values (start-an-identifier-p in)
	      (read-char in))))
:values (t #\-)

#?(with-input-from-string (in "\\hoge")
    (let ((in (read-css::ensure-input-stream in)))
      (values (start-an-identifier-p in)
	      (read-char in))))
:values (t #\\)

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
,:test equal

; With escape.
#?(with-input-from-string (in "foo\\26 bar")
    (consume-a-name in))
=> "foo&bar"
,:test equal
#?(with-input-from-string (in "foo\\000026bar")
    (consume-a-name in))
=> "foo&bar"
,:test equal
#?(with-input-from-string (in "foo\\0000267bar")
    (consume-a-name in))
=> "foo&7bar"
,:test equal

#?(with-input-from-string (in "-+foo") (consume-a-name in))
=> "-"
,:test equal

#?(with-input-from-string (in (format nil "-\\~%foo")) (consume-a-name in))
=> "-"
,:test equal

#?(with-input-from-string (in (format nil "1234")) (consume-a-name in))
=> "1234"
,:test equal

#?(with-input-from-string (in "foo:bar")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-name in)
	      (read-char in))))
:values ("foo" #\:)

#?(with-input-from-string (in "   name-with-white-spaces")
    (consume-a-name in))
=> "name-with-white-spaces"
,:test equal

(requirements-about CONSUME-SELECTORS :doc-type function)

;;;; Description:

#+syntax (CONSUME-SELECTORS input first-char) ; => result

;;;; Arguments and Values:

; input := css-input-stream

; first-char := character

; result := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "commentBox{")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-selectors in #\.)
	      (read-char in))))
:values ((".commentBox") #\{)

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
#?(with-input-from-string (in "1 2")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-number in) (read-char in))))
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
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1)))
#?(with-input-from-string (in "+1") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1)))
#?(with-input-from-string (in "-1") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value -1)))

; Float.
#?(with-input-from-string (in "1.0") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1.0)))
#?(with-input-from-string (in "+1.0") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1.0)))
#?(with-input-from-string (in "-1.0") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value -1.0)))

; Exponention.
#?(with-input-from-string (in "1.0e5") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1.0e5)))
#?(with-input-from-string (in "+1.0e+5") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1.0e+5)))
#?(with-input-from-string (in "-1.0e-5") (consume-a-numeric-token in))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value -1.0e-5)))

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

; em
#?(with-input-from-string (in "1.5em") (consume-a-numeric-token in))
:satisfies (lambda (result)
	     (& (equalp result (read-css::make-dimension-token
				 :value 1.5
				 :unit "em"))))

(requirements-about CONSUME-A-STRING-TOKEN :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-STRING-TOKEN character &optional (input *standard-input*))
; => result

;;;; Arguments and Values:

; character := 

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Case escaped backquote.
#?(with-input-from-string (in "\\\"\"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal "\"" (read-css::string-token-value result))))

; Case hex digits just followed end char.
#?(with-input-from-string (in "\\22\"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal "\"" (read-css::string-token-value result))))

; Case hex digits just followed space.
#?(with-input-from-string (in "\\22 \"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal "\"" (read-css::string-token-value result))))

; Case hex digits max.
#?(with-input-from-string (in "\\0000278\"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal "'8" (read-css::string-token-value result))))

; Case copyright symbol.
#?(with-input-from-string (in "\\A9\"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal #.(string (code-char #xA9))
		       (read-css::string-token-value result))))

; Case backslash.
#?(with-input-from-string (in "\\\\\"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal "\\" (read-css::string-token-value result))))

; Case newline.
#?(with-input-from-string (in "foo\\A bar\"") (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal #.(format nil "foo~%bar")
		       (read-css::string-token-value result))))

; Case string spanning.
#?(with-input-from-string (in (format nil "foo\\~%bar\"")) (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::string-token)
		(equal "foobar" (read-css::string-token-value result))))

; Case missing end char.
#?(with-input-from-string (in "") (consume-a-string-token #\" in))
:signals read-css::css-parse-error
#?(with-input-from-string (in "") (consume-a-string-token #\" in))
:invokes-debugger not

; Case bad newline.
#?(with-input-from-string (in (format nil "foo~%bar\"")) (consume-a-string-token #\" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::bad-string-token)
		(equal "foo" (read-css::bad-string-token-value result))))
,:ignore-signals nil

(requirements-about CONSUME-AN-IDENT-LIKE-TOKEN :doc-type function)

;;;; Description:

#+syntax (CONSUME-AN-IDENT-LIKE-TOKEN &optional (input *standard-input*))
; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Token.
#?(with-input-from-string (in "token") (consume-an-ident-like-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::ident-token)
		(equal "token" (read-css::ident-token-value result))))

; Function.
#?(with-input-from-string (in "function()") (consume-an-ident-like-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::function-token)
		(equal "function" (read-css::function-token-name result))
		(equal () (read-css::function-token-args result))))

; Url
#?(with-input-from-string (in "url(https://example.com/images/myImg.jpg)")
    (consume-an-ident-like-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::url-token)
		(equal "https://example.com/images/myImg.jpg"
		       (read-css::url-token-value result))))

#?(with-input-from-string (in "url(#IDofSVGPath)")
    (consume-an-ident-like-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::url-token)
		(equal "#IDofSVGPath"
		       (read-css::url-token-value result))))

#?(with-input-from-string (in "hsla(0,0%,95%,1.00);")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-an-ident-like-token in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (function-token char)
  (& (equalp (read-css::make-function-token :name "hsla"
					    :args (list (read-css::make-number-token :value 0)
							(read-css::make-percentage-token :value 0)
							(read-css::make-percentage-token :value 95)
							(read-css::make-number-token :value 1.00)))
	     function-token)
     (eql #\; char)))

(requirements-about CONSUME-A-SIMPLE-BLOCK :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-SIMPLE-BLOCK end-char input) ; => result

;;;; Arguments and Values:

; end-char := (or null character)

; input := css-input-stream

; result := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "   \"hoge\"")
    (let ((*readtable* (named-readtables:find-readtable 'read-css::css-readtable)))
      (consume-a-simple-block nil (read-css::ensure-input-stream in))))
:satisfies (lambda (result)
	     (& (listp result)
		(= 1 (length result))
		(typep (car result) 'read-css::string-token)
		(equal "hoge" (read-css::string-token-value (car result)))))
,:ignore-signals nil

#?(with-input-from-string (in "a,b,c)d")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-simple-block #\) in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (list char)
  (& (equalp list
	     (list (read-css::make-ident-token :value "a")
		   (read-css::make-ident-token :value "b")
		   (read-css::make-ident-token :value "c")))
     (eql char #\d)))

#?(with-input-from-string (in "}a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-simple-block #\} in)
	      (read-char in))))
:values (() #\a)

(requirements-about CONSUME-A-FUNCTION :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-FUNCTION name &optional (input *standard-input*))
; => result

;;;; Arguments and Values:

; name := 

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Null args.
#?(with-input-from-string (in ")") (consume-a-function "fun-name" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::function-token)
		(equal "fun-name" (read-css::function-token-name result))
		(null (read-css::function-token-args result))))

; Some args
#?(with-input-from-string (in "a,b)") (consume-a-function "fun-name" in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::function-token)
		(equal "fun-name" (read-css::function-token-name result))
		(equalp (list (read-css::make-ident-token :value "a")
			      (read-css::make-ident-token :value "b"))
			(read-css::function-token-args result))))

; Close paren will be consumed.
#?(with-input-from-string (in ")a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-function "fun-name" in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (fun-token char)
  (& (equalp (read-css::make-function-token :name "fun-name"
					    :args nil)
	     fun-token)
     (eql #\a char)))

(requirements-about |#rgb-reader| :doc-type function)

;;;; Description:

#+syntax (|#rgb-reader| input hash) ; => result

;;;; Arguments and Values:

; input := 

; hash := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "12345678")
    (values (|#rgb-reader| in #\#)
	    (read-char in)))
:multiple-value-satisfies
(lambda (color char)
  (& (equalp color (cl-colors2:rgb (float (/ #x12 #xFF))
				   (float (/ #x34 #xFF))
				   (float (/ #x56 #xFF))))
     (eql char #\7)))

(requirements-about |{-reader| :doc-type function)

;;;; Description:

#+syntax (|{-reader| input open-paren) ; => result

;;;; Arguments and Values:

; input := 

; open-paren := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:
#?(with-input-from-string (in "padding-top:6px;}a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (|{-reader| in #\{)
	      (read-char in))))
:multiple-value-satisfies
(lambda (list char)
  (& (equalp (list (read-css::make-css-declaration
		     :name "padding-top"
		     :list (list (list (read-css::make-dimension-token :value 6 :type nil :unit "px")))))
	     list)
     (eql char #\a)))

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
:signals read-css::css-parse-error

;;; Numbers
; Integer
#?(with-input-from-string (in "0")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 0))))

; Signed integer.
#?(with-input-from-string (in "+1")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 1))))

#?(with-input-from-string (in "-1")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value -1))))

#?(with-input-from-string (in "+1 -1")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 1)
				       (read-css::make-number-token :value -1))))

; Float.
#?(with-input-from-string (in "0.5")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 0.5))))

; Signed float.
#?(with-input-from-string (in "+0.5 -0.5")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 0.5)
				       (read-css::make-number-token :value -0.5))))

; Starts with dot.
#?(with-input-from-string (in ".5 +.5 -.5")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 0.5)
				       (read-css::make-number-token :value 0.5)
				       (read-css::make-number-token :value -0.5))))

; Exponential.
#?(with-input-from-string (in "0.5e-3 0.5E-3 +0.5e-3 -0.5E-3 .5e3 .5E3")
    (read-css in))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 0.5e-3)
				       (read-css::make-number-token :value 0.5e-3)
				       (read-css::make-number-token :value 0.5e-3)
				       (read-css::make-number-token :value -0.5e-3)
				       (read-css::make-number-token :value .5e3)
				       (read-css::make-number-token :value .5e3))))

#?(with-input-from-string (in ".jishin")
    (read-css in))
:signals parse-error

#?(with-input-from-string (in ".jishin{}")
    (read-css in))
:satisfies (lambda (result)
	     (equalp result (list (read-css::make-qualified-rule
				    :selectors '(".jishin")
				    :declarations ()))))

#?(with-input-from-string (in ".jishin {background:hsla(0,0%,95%,1.00); height:65px; padding-top:6px;}")
    (read-css in))
:satisfies
(lambda (result)
  (& (equalp result
	     (list (read-css::make-qualified-rule
		     :selectors '(".jishin")
		     :declarations
		     (list (read-css::make-css-declaration
			     :name "background"
			     :list
			     (list (list (read-css::make-function-token
					   :name "hsla"
					   :args (list (read-css::make-number-token :value 0)
						       (read-css::make-percentage-token :value 0)
						       (read-css::make-percentage-token :value 95)
						       (read-css::make-number-token :value 1.0))))))
			   (read-css::make-css-declaration
			     :name "height"
			     :list (list (list (read-css::make-dimension-token :value 65 :type nil :unit "px"))))
			   (read-css::make-css-declaration
			     :name "padding-top"
			     :list (list (list (read-css::make-dimension-token :value 6 :type nil :unit "px"))))))))))

#?(with-input-from-string (in ".commentBox{ border: 1px solid #CCC; padding: 15px 153px;}")
    (read-css in))
:satisfies (lambda (result)
	     (& (equalp result
			(list (read-css::make-qualified-rule
				:selectors '(".commentBox")
				:declarations
				(list (read-css::make-css-declaration
					:name "border"
					:list (list (list (read-css::make-dimension-token
							    :value 1 :type nil :unit "px")
							  (read-css::make-ident-token :value "solid")
							  (cl-colors2:rgb 0.8 0.8 0.8))))
				      (read-css::make-css-declaration
					:name "padding"
					:list (list (list (read-css::make-dimension-token
							    :value 15 :type nil :unit "px")
							  (read-css::make-dimension-token
							    :value 153 :type nil :unit "px"))))))))))

#?(with-input-from-string (in "{ background: #ffffe2; border-radius:10px;}")
    (read-css in))
:satisfies (lambda (x)
	     (& (equalp x `((,(read-css::make-css-declaration
				:name "background"
				:list `((,(cl-colors2:rgb 1.0 1.0 0.8862745))))
			      ,(read-css::make-css-declaration
				 :name "border-radius"
				 :list `((,(read-css::make-dimension-token
					     :value 10
					     :type nil
					     :unit "px")))))))))

#?(with-input-from-string (in "{background-size: 200px auto, 44px 76px;}")
    (read-css in))
:satisfies (lambda (x)
	     (& (equalp x
			`((,(read-css::make-css-declaration
			      :name "background-size"
			      :list `((,(read-css::make-dimension-token
					  :value 200
					  :unit "px")
					,(read-css::make-ident-token
					   :value "auto"))
				      (,(read-css::make-dimension-token
					  :value 44
					  :unit "px")
				       ,(read-css::make-dimension-token
					  :value 76
					  :unit "px")))))))))

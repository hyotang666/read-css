(defpackage :read-css.spec
  (:use :cl :jingoh :read-css)
  (:import-from :read-css "#rgb-reader" "{-reader"))
(in-package :read-css.spec)
(setup :read-css)

(requirements-about common-lisp)

#.(or ; To avoid #-.
    #+(or clisp)
    '#?(with-input-from-string (in "abcd")
	 (let ((a (read-char in))
	       (b (read-char in)))
	   (unread-char b in)
	   (unread-char a in)))
    :signals error
    ;; The default
    '#?(with-input-from-string (in "abcd")
	 (let ((a (read-char in))
	       (b (read-char in)))
	   (unread-char b in)
	   (unread-char a in)
	   (read-char in)))
    => #\a)

(requirements-about START-A-NUMBER-P :doc-type function)

;;;; Description:

#+syntax (START-A-NUMBER-P input) ; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "+1") (start-a-number-p in)) => T
#?(with-input-from-string (in "-1") (start-a-number-p in)) => T
#?(with-input-from-string (in "+") (start-a-number-p in)) => NIL
#?(with-input-from-string (in "-") (start-a-number-p in)) => NIL
#?(with-input-from-string (in "+.1") (start-a-number-p in)) => T
#?(with-input-from-string (in "-.1") (start-a-number-p in)) => T
#?(with-input-from-string (in "+.N") (start-a-number-p in)) => NIL
#?(with-input-from-string (in "-.N") (start-a-number-p in)) => NIL
#?(with-input-from-string (in ".1") (start-a-number-p in)) => T
#?(with-input-from-string (in ".N") (start-a-number-p in)) => NIL
#?(with-input-from-string (in "1") (start-a-number-p in)) => T
#?(with-input-from-string (in "N") (start-a-number-p in)) => NIL

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

#?(with-input-from-string (in "-N") (start-an-identifier-p in)) => T
#?(with-input-from-string (in "--") (start-an-identifier-p in)) => T
#?(with-input-from-string (in "-\\N") (start-an-identifier-p in)) => T
#?(with-input-from-string (in "-0") (start-an-identifier-p in)) => NIL
#?(with-input-from-string (in "-$") (start-an-identifier-p in)) => NIL
#?(with-input-from-string (in "$") (start-an-identifier-p in)) => NIL

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
#?(with-input-from-string (in "missing \\*\\/")
    (consume-comments in))
:signals end-of-file

;;;; Tests:
#?(with-input-from-string (in " comment */a")
    (values (consume-comments in)
	    (read-char in)))
:values (nil #\a)

; Return (values)
#?(with-input-from-string (in "comment */")
    (multiple-value-call (lambda (&rest returned)
			   (null returned))
      (consume-comments in)))
=> T

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
#?(with-input-from-string (in "")
    (consume-a-name in))
:signals parse-error
#?(with-input-from-string (in "+")
    (consume-a-name in))
:signals parse-error

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

#?(with-input-from-string (in "-1-2") (consume-a-number in)) => -1

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

#?(with-input-from-string (in "1.5 em") (consume-a-numeric-token in))
:satisfies (lambda (result)
	     (& (equalp result (read-css::make-number-token
				 :value 1.5))))

#?(with-input-from-string (in "-1-2") (consume-a-numeric-token in))
:satisfies (lambda (result)
	     (& (equalp result (read-css::make-number-token
				 :value -1))))

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
#?(with-input-from-string (in "    https://example.com/images/myImg.jpg)")
    (consume-a-url-token in))
:satisfies (lambda (result)
	     (& (typep result 'read-css::url-token)
		(equal "https://example.com/images/myImg.jpg"
		       (read-css::url-token-value result))))

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

; Case empty url.
#?(with-input-from-string (in ")")
    (consume-a-url-token in))
:satisfies (lambda (result)
	     (& (equalp result (read-css::make-url-token :value ""))))

; Case end-of-file.
#?(with-input-from-string (in "") (consume-a-url-token in))
:signals end-of-file
,:with-restarts continue

; Case parse error.
#?(with-input-from-string (in "\"") (consume-a-url-token in))
:signals parse-error
,:with-restarts continue

#?(with-input-from-string (in "'") (consume-a-url-token in))
:signals parse-error
,:with-restarts continue

#?(with-input-from-string (in "(") (consume-a-url-token in))
:signals parse-error
,:with-restarts continue

#?(with-input-from-string (in (format nil "\\~%")) (consume-a-url-token in))
:signals parse-error
,:with-restarts continue

#?(loop :for c :in read-css::+non-printable-code-point+
	:always (handler-case (with-input-from-string (in (string c))
				(consume-a-url-token in))
		  (parse-error () t)
		  (:no-error (token)
                    (declare (ignore token))
		    nil)))
=> T

#?(handler-bind ((end-of-file #'continue))
    (with-input-from-string (in "") (consume-a-url-token in)))
:satisfies (lambda (result)
	     (& (equalp result
			(read-css::make-url-token
			  :value ""))))

#?(handler-bind ((parse-error #'continue))
    (with-input-from-string (in "\"") (consume-a-url-token in)))
:satisfies (lambda (result)
	     (& (equalp result
			(read-css::make-bad-url-token
			  :value ""))))

#?(handler-bind ((parse-error #'continue))
    (with-input-from-string (in "'") (consume-a-url-token in)))
:satisfies (lambda (result)
	     (& (equalp result
			(read-css::make-bad-url-token
			  :value ""))))

#?(handler-bind ((parse-error #'continue))
    (with-input-from-string (in "(") (consume-a-url-token in)))
:satisfies (lambda (result)
	     (& (equalp result
			(read-css::make-bad-url-token
			  :value ""))))

#?(with-input-from-string (in "\\26)") (consume-a-url-token in))
:satisfies (lambda (result)
	     (& (equalp result
			(read-css::make-url-token
			  :value "&"))))

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
:signals parse-error
,:with-restarts continue

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
	     (& (equal "token" result)))

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
  (& (equalp (read-css::make-function-token
	       :name "hsla"
	       :args (list (read-css::make-number-token :value 0)
			   (read-css::make-percentage-token :value 0)
			   (read-css::make-percentage-token :value 95)
			   (read-css::make-number-token :value 1.00)))
	     function-token)
     (eql #\; char)))

(requirements-about CONSUME-COMPONENTS :doc-type function)

;;;; Description:

#+syntax (CONSUME-COMPONENTS end-chars input) ; => result

;;;; Arguments and Values:

; end-chars := sequence

; input := css-input-stream

; result := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "6px;")
    (let ((*readtable* (named-readtables:find-readtable 'read-css::css-non-toplevel))
	  (in (read-css::ensure-input-stream in)))
      (consume-components ";" in)))
:satisfies (lambda (list)
	     (& (equalp list
			`((,(read-css::make-dimension-token
			      :value 6
			      :unit "px"))))))

#?(with-input-from-string (in "screen and (max-width: 991px){")
    (let ((in (read-css::ensure-input-stream in)))
      (consume-components ";{" in)))
:satisfies (lambda (result)
	     (& (equalp result
			`(("screen" "and"
			   (,(read-css::make-css-declaration
			       :name "max-width"
			       :list `((,(read-css::make-dimension-token
					   :value 991
					   :unit "px"))))))))))

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

#?(with-input-from-string (in "{")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-selectors in #\p)
	      (read-char in))))
:values (("p") #\{)

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
	     (& (equalp result
			(list (read-css::make-string-token
				:value "hoge")))))
,:ignore-signals nil

#?(with-input-from-string (in "a,b,c)d")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-simple-block #\) in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (list char)
  (& (equalp list '("a" "b" "c"))
     (eql char #\d)))

#?(with-input-from-string (in "}a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-simple-block #\} in)
	      (read-char in))))
:values (() #\a)

#?(with-input-from-string (in "1,1%,95%,1.00)a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-simple-block #\) in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (list char)
  (& (equalp list
	     (list (read-css::make-number-token :value 1)
		   (read-css::make-percentage-token :value 1)
		   (read-css::make-percentage-token :value 95)
		   (read-css::make-number-token :value 1.0)))
     (eql char #\a)))

#?(with-input-from-string (in "1,0%,95%,1.00)a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (read-style in nil nil t)
	      (read-char in))))
:multiple-value-satisfies
(lambda (token char)
  (& (equalp token (read-css::make-number-token :value 1))
     (eql #\, char)))

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
		(equalp '("a" "b") (read-css::function-token-args result))))

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

#?(with-input-from-string (in "100% + 7px);}")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-function "calc" in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (token char)
  (& (equalp token
	     (read-css::make-function-token
	       :name "calc"
	       :args (list (read-css::make-percentage-token :value 100)
			   (read-css::make-delim-token :value "+")
			   (read-css::make-dimension-token :value 7
							   :unit "px"))))
     (eql char #\;)))

(requirements-about CONSUME-A-DECLARATION :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-DECLARATION &optional (input *standard-input*)) ; => result

;;;; Arguments and Values:

; input := (or boolean stream)

; result := (or null css-declaration)

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "padding-top:6px;")
    (let ((*readtable* (named-readtables::find-readtable 'read-css::css-non-toplevel))
	  (in (read-css::ensure-input-stream in)))
      (consume-a-declaration #\} in)))
:satisfies (lambda (decl)
	     (& (equalp decl
			(read-css::make-css-declaration
			  :name "padding-top"
			  :list `((,(read-css::make-dimension-token
				      :value 6
				      :unit "px")))))))

(requirements-about CONSUME-A-LIST-OF-DECLARATIONS :doc-type function)

;;;; Description:

#+syntax (CONSUME-A-LIST-OF-DECLARATIONS &optional (input *standard-input*))
; => result

;;;; Arguments and Values:

; input := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(with-input-from-string (in "padding-top:6px;}a")
    (let ((in (read-css::ensure-input-stream in)))
      (values (consume-a-list-of-declarations #\} in)
	      (read-char in))))
:multiple-value-satisfies
(lambda (list char)
  (& (equalp (list (read-css::make-css-declaration
		     :name "padding-top"
		     :list (list (list (read-css::make-dimension-token :value 6 :unit "px")))))
	     list)
     (eql char #\a)))

#?(with-input-from-string (in " background: #ffffe2; border-radius:10px;}")
    (consume-a-list-of-declarations #\} in))
:satisfies (lambda (x)
	     (& (equalp x `(,(read-css::make-css-declaration
			       :name "background"
			       :list `((,(cl-colors2:rgb 1 1 226/255))))
			     ,(read-css::make-css-declaration
				:name "border-radius"
				:list `((,(read-css::make-dimension-token
					    :value 10
					    :unit "px"))))))))

#?(with-input-from-string (in "background-size: 200px auto, 44px 76px;}")
    (consume-a-list-of-declarations #\} in))
:satisfies (lambda (x)
	     (& (equalp x
			`(,(read-css::make-css-declaration
			     :name "background-size"
			     :list `((,(read-css::make-dimension-token
					 :value 200
					 :unit "px")
				       ,"auto")
				     (,(read-css::make-dimension-token
					 :value 44
					 :unit "px")
				       ,(read-css::make-dimension-token
					  :value 76
					  :unit "px"))))))))

#?(with-input-from-string (in "/* overflow:auto; */height:250px;position:relative;}")
    (consume-a-list-of-declarations #\} in))
:satisfies (lambda (x)
	     (& (equalp x
			`(,(read-css::make-css-declaration
			     :name "height"
			     :list `((,(read-css::make-dimension-token
					 :value 250
					 :unit "px"))))
			   ,(read-css::make-css-declaration
			      :name "position"
			      :list `(("relative")))))))

#?(with-input-from-string (in " font-size:87.5%; }")
    (consume-a-list-of-declarations #\} in))
:satisfies (lambda (x)
	     (& (equalp x
			(list (read-css::make-css-declaration
				:name "font-size"
				:list `((,(read-css::make-percentage-token
					    :value 87.5))))))))

#?(with-input-from-string (in "opacity:0!important;}")
    (consume-a-list-of-declarations #\} in))
:satisfies
(lambda (x)
  (& (equalp x (list (read-css::make-css-declaration
		       :name "opacity"
		       :importantp t
		       :list `((,(read-css::make-number-token :value 0))))))))

#?(with-input-from-string (in "filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#ffffff', endColorstr='#b3b3b3',GradientType=0 ); }")
    (consume-a-list-of-declarations #\} in))
:signals parse-error ; duplicated colon.
,:with-restarts continue

#?(with-input-from-string (in "filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#ffffff', endColorstr='#b3b3b3',GradientType=0 ); }")
    (handler-bind ((parse-error #'continue))
      (values (consume-a-list-of-declarations #\} in)
	      (read-char in nil nil))))
:values (nil nil)
,:ignore-signals warning

#?(with-input-from-string (in "top: calc(100% + 7px);}")
    (consume-a-list-of-declarations #\} in))
:satisfies
(lambda (x)
  (& (equalp x
	     (list (read-css::make-css-declaration
		     :name "top"
		     :list `((,(read-css::make-function-token
				 :name "calc"
				 :args (list (read-css::make-percentage-token
					       :value 100)
					     (read-css::make-delim-token
					       :value "+")
					     (read-css::make-dimension-token
					       :value 7
					       :unit "px"))))))))))

#?(with-input-from-string (in "max-width: 991px}")
    (consume-a-list-of-declarations #\} in))
:satisfies (lambda (token)
	     (& (equalp token
			(list (read-css::make-css-declaration
				:name "max-width"
				:list `((,(read-css::make-dimension-token
					    :value 991
					    :unit "px"))))))))

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
  (& (equalp color (cl-colors2:rgb (/ #x12 #xFF)
				   (/ #x34 #xFF)
				   (/ #x56 #xFF)))
     (eql char #\7)))

(requirements-about READ-STYLE :doc-type function)

;;;; Description:
; Read one css style from specified input.

#+syntax (READ-STYLE &optional
           (input *standard-input*)
           (eof-error-p t)
           eof-value
           recursive-p)
; => result

;;;; Arguments and Values:

; input := (or boolean stream), otherwise implementation dependent condition.
#?(read-style "not stream-designator") :signals condition

; eof-error-p := boolean
; If true (the default), signals end-of-file when get end of file.
#?(with-input-from-string (in "")
    (read-style in))
:signals end-of-file

; If NIL, end-of-file is not signaled.
#?(with-input-from-string (in "") (read-style in nil)) :invokes-debugger not

; eof-value := t
; If EOF-ERROR-P is NIL and get end-of-file, this value is returned.
#?(with-input-from-string (in "") (read-style in nil :this-is-returned))
=> :THIS-IS-RETURNED

; recursive-p := boolean
; Internal use for controling readtable.
; e.g. '#' is used for id tag in toplevel but
; color code in non toplevel.

#?(with-input-from-string (in "0")
    (read-style in))
:signals error

; result := t

;;;; Affected By:

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
;;; Comment.
#?(with-input-from-string (in "/* comment */")
    (read-style in))
:signals end-of-file

; (Unfortunately) nested comment is disabled.
#?(with-input-from-string (in "/* nested /* comment */ is disabled */")
    (read-style in t t t))
=> "is"
,:test equal

;;; Numbers
; Integer
#?(with-input-from-string (in "0")
    (read-style in t t t))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 0)))

; Signed integer.
#?(with-input-from-string (in "+1")
    (read-style in t t t))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 1)))

#?(with-input-from-string (in "-1")
    (read-style in t t t))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value -1)))

; Float.
#?(with-input-from-string (in "0.5")
    (read-style in t t t))
:satisfies (lambda (x) (equalp x (read-css::make-number-token :value 0.5)))

; Signed float.
#?(with-input-from-string (in "+0.5 -0.5")
    (values (read-style in t t t)
	    (read-style in t t t)))
:multiple-value-satisfies
(lambda (a b)
  (& (equalp a (read-css::make-number-token :value 0.5))
     (equalp b (read-css::make-number-token :value -0.5))))

; Starts with dot.
#?(with-input-from-string (in ".5 +.5 -.5")
    (values (read-style in t t t)
	    (read-style in t t t)
	    (read-style in t t t)))
:multiple-value-satisfies
(lambda (a b c)
  (& (equalp a (read-css::make-number-token :value 0.5))
     (equalp b (read-css::make-number-token :value 0.5))
     (equalp c (read-css::make-number-token :value -0.5))))

; Exponential.
#?(with-input-from-string (in "0.5e-3 0.5E-3 +0.5e-3 -0.5E-3 .5e3 .5E3")
    (loop :repeat 6
	  :collect (read-style in t t t)))
:satisfies (lambda (x) (equalp x (list (read-css::make-number-token :value 0.5e-3)
				       (read-css::make-number-token :value 0.5e-3)
				       (read-css::make-number-token :value 0.5e-3)
				       (read-css::make-number-token :value -0.5e-3)
				       (read-css::make-number-token :value .5e3)
				       (read-css::make-number-token :value .5e3))))

; Missing declarations.
#?(with-input-from-string (in ".jishin")
    (read-style in))
:signals parse-error

; Syntax error as property name.
#?(with-input-from-string (in ".jishin")
    (read-style in t t t))
:signals parse-error

#?(with-input-from-string (in "jishin")
    (read-style in t t t))
=> "jishin"
,:test equal

#?(with-input-from-string (in ".jishin{}")
    (read-style in))
:satisfies (lambda (result)
	     (equalp result (read-css::make-qualified-rule
			      :selectors '(".jishin")
			      :declarations ())))

#?(with-input-from-string (in ".jishin {background:hsla(0,0%,95%,1.00); height:65px; padding-top:6px;}")
    (read-style in))
:satisfies
(lambda (result)
  (& (equalp result
	     (read-css::make-qualified-rule
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
		       :list (list (list (read-css::make-dimension-token :value 65 :unit "px"))))
		     (read-css::make-css-declaration
		       :name "padding-top"
		       :list (list (list (read-css::make-dimension-token :value 6 :unit "px")))))))))

#?(with-input-from-string (in ".commentBox{ border: 1px solid #CCC; padding: 15px 153px;}")
    (read-style in))
:satisfies (lambda (result)
	     (& (equalp result
			(read-css::make-qualified-rule
			  :selectors '(".commentBox")
			  :declarations
			  (list (read-css::make-css-declaration
				  :name "border"
				  :list (list (list (read-css::make-dimension-token
						      :value 1 :unit "px")
						    "solid"
						    (cl-colors2:rgb 4/5 4/5 4/5))))
				(read-css::make-css-declaration
				  :name "padding"
				  :list (list (list (read-css::make-dimension-token
						      :value 15 :unit "px")
						    (read-css::make-dimension-token
						      :value 153 :unit "px")))))))))

#?(with-input-from-string (in "p{ /*comment*/ }")
    (let ((in (read-css::ensure-input-stream in)))
      (values (read-style in)
	      (read-char in nil nil))))
:multiple-value-satisfies
(lambda (x c)
  (& (equalp x (read-css::make-qualified-rule
		 :selectors '("p")
		 :declarations nil))
     (null c)))

; Missing last semi-colon is valid.
#?(with-input-from-string (in ".txtSizeSS{ font-size:74%}")
    (read-style in))
:satisfies
(lambda (x)
  (& (equalp x (read-css::make-qualified-rule
		 :selectors '(".txtSizeSS")
		 :declarations (list (read-css::make-css-declaration
				       :name "font-size"
				       :importantp nil
				       :list (list (list (read-css::make-percentage-token
							   :value 74)))))))))

#?(with-input-from-string (in "@import url(\"http://function/style/url\");")
    (read-style in))
:satisfies (lambda (rule)
	     (& (equalp rule
			(read-css::make-at-rule
			  :name "import"
			  :components `((,(read-css::make-function-token
					    :name "url"
					    :args (list (read-css::make-string-token
							  :value "http://function/style/url")))))
			  :block nil))))

#?(with-input-from-string (in " @media screen and (max-width: 991px) { } ")
    (read-style in))
:satisfies (lambda (rule)
	     (& (equalp rule
			(read-css::make-at-rule
			  :name "media"
			  :components `(("screen" "and"
					 (,(read-css::make-css-declaration
					     :name "max-width"
					     :list `((,(read-css::make-dimension-token
							 :value 991
							 :unit "px")))))))))))

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

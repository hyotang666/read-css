(in-package :cl-user)

(defpackage :read-css
  (:use :cl)
  #| Main API for light users. |#
  (:export #:read-css #:read-style #:read-style-from-string)
  #| API for heavy users. |#
  (:export ;;;; Conditions.
           #:css-parse-error
           #:end-of-css
           #:invalid-escape
           #:simple-parse-error
           #:name-parse-error
           ;; Slot reader.
           #:parse-error-character)
  (:export ;;;; Style rules.
           ;;; QUALIFIED-RULE
           #:qualified-rule ; type name.
           ;; accessors.
           #:qualified-rule-selectors
           #:qualified-rule-declarations
           ;;; AT-RULE
           #:at-rule
           ;; accessors
           #:at-rule-name
           #:at-rule-components
           #:at-rule-block)
  (:export ;;;; CSS-DECLARATION
           #:css-declaration ; type name.
           ;; Slot accessors.
           #:css-declaration-name
           #:css-declaration-importantp
           #:css-declaration-list)
  (:export ;;;; Tokens.
           #:css-token ; as super-class
           #:string-token
           #:bad-string-token
           #:number-token
           #:percentage-token
           #:dimension-token
           #:url-token
           #:bad-url-token
           #:function-token
           #:important-token
           #:delim-token
           #:at-keyword-token
           ;; Slot accessors.
           #:string-token-value
           #:number-token-value
           #:dimension-token-unit
           #:function-token-name
           #:function-token-args)
  (:export ;;;; READTABLE-NAME
           #:css-readtable)
  #| API for hackers. |#
  (:export ;;;; CONSUMERS.
           #:consume-a-declaration
           #:consume-a-function
           #:consume-a-list-of-declarations
           #:comsume-a-name
           #:consume-a-number
           #:consume-a-numric-token
           #:consume-a-simple-block
           #:consume-a-string-token
           #:consume-a-url-token
           #:consume-an-escaped-code-point
           #:consume-an-ident-like-token
           #:consume-comments
           #:consume-components
           #:consume-selectors
           #:consume-the-remnants-of-a-bad-url)
  (:export ;;;; Predicates.
           #:non-ascii-code-point-p
           #:name-start-code-point-p
           #:name-code-point-p
           #:non-printable-code-point-p
           #:white-space-p
           #:surrogatep
           #:valid-escape-p
           #:start-a-number-p
           #:start-an-identifier-p))

(in-package :read-css)

;;;; CSS-INPUT-STREAM
; To enable multitime UNREAD-CHAR.

(defclass css-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((stream :initarg :stream :reader css-input-stream)
   (kept :initform nil :accessor kept-chars)))

(defmethod trivial-gray-streams:stream-read-char ((input css-input-stream))
  (or (pop (kept-chars input)) (read-char (css-input-stream input) nil :eof)))

(defmethod trivial-gray-streams:stream-unread-char
           ((input css-input-stream) (c character))
  (push c (kept-chars input))
  nil)

(defmethod trivial-gray-streams:stream-file-position ((input css-input-stream))
  (file-position (css-input-stream input)))

;;;; ABSTRUCT STRUCTURE

(defstruct css-token)

(defstruct (string-token (:include css-token))
  (value (error "VALUE is required.") :type string))

(defstruct (number-token (:include css-token))
  (value (error "VALUE is required.") :type real))

;;;; CONDITIONS

(define-condition css-parse-error (parse-error) ())

(define-condition end-of-css (end-of-file css-parse-error) ())

(define-condition end-of-declaration (end-of-css)
  ((styles :initarg :declarations :reader styles)))

(define-condition invalid-escape (css-parse-error)
  ((char :initarg :character :reader parse-error-character))
  (:report
   (lambda (this output)
     (funcall (formatter "Escape immediately follows ~S is invalid.") output
              (parse-error-character this)))))

(define-condition simple-parse-error (css-parse-error simple-condition) ())

(define-condition name-parse-error (css-parse-error)
  ((char :initarg :character :reader parse-error-character))
  (:report
   (lambda (this output)
     (funcall (formatter "Could not consume a name. ~S") output
              (parse-error-character this)))))

(define-condition internal-logical-error (simple-error) ())

(defun internal-logical-error (format-control &rest format-args)
  (error 'internal-logical-error
         :format-control format-control
         :format-arguments format-args))

;;;; Utilities

(defun ensure-input-stream (stream-designator)
  (etypecase stream-designator
    (css-input-stream stream-designator)
    (stream (make-instance 'css-input-stream :stream stream-designator))
    ((eql t) (make-instance 'css-input-stream :stream *terminal-io*))
    (null (make-instance 'css-input-stream :stream *standard-input*))))

(defun non-ascii-code-point-p (char)
  ;; https://www.w3.org/TR/css-syntax-3/#non-ascii-code-point
  (<= #x80 (char-code char)))

(declaim
 (ftype (function (string css-input-stream) (values boolean &optional))
        stream-start-with))

(defmacro unread-protect ((var input) &body body)
  (let ((stream (gensym "INPUT")))
    `(let* ((,stream ,input) (,var (read-char ,stream nil nil)))
       (unwind-protect (progn ,@body)
         (when ,var
           (unread-char ,var ,stream))))))

(defun stream-start-with (string input)
  (labels ((rec (index)
             (if (not (array-in-bounds-p string index))
                 t
                 (unread-protect (char input)
                   (if (eql char (aref string index))
                       (rec (1+ index))
                       nil)))))
    (rec 0)))

(defun remove-if-find
       (function list &aux (function (coerce function 'function)))
  (let* ((head (cons :head nil)) (tail head) (found?))
    (labels ((rec (list)
               (if (endp list)
                   (values (cdr head) found?)
                   (body (car list) (cdr list))))
             (body (elt rest)
               (if (funcall function elt)
                   (progn (setf found? t) (rec rest))
                   (progn (rplacd tail (setf tail (list elt))) (rec rest)))))
      (rec list))))

;;;; CONSTANTS

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Predicates below needs this eval-when.
  ;; https://www.w3.org/TR/css-syntax-3/#digit
  (unless (boundp '+digits+)
    (defconstant +digits+ "1234567890"))
  ;; https://www.w3.org/TR/css-syntax-3/#uppercase-letter
  (unless (boundp '+uppercase-letters+)
    (defconstant +uppercase-letters+
      (coerce
        (loop :for code :upfrom (char-code #\A) :to (char-code #\Z)
              :collect (code-char code))
        'string)))
  ;; https://www.w3.org/TR/css-syntax-3/#lowercase-letter
  (unless (boundp '+lowercase-letters+)
    (defconstant +lowercase-letters+
      (coerce
        (loop :for code :upfrom (char-code #\a) :to (char-code #\z)
              :collect (code-char code))
        'string)))
  ;; https://www.w3.org/TR/css-syntax-3/#letter
  (unless (boundp '+letters+)
    (defconstant +letters+
      (concatenate 'string +uppercase-letters+ +lowercase-letters+)))
  ;; https://www.w3.org/TR/css-syntax-3/#newline
  (unless (boundp '+newlines+)
    (defconstant +newlines+ (coerce '(#\Newline #\Return #\Page) 'string)))
  ;; https://www.w3.org/TR/css-syntax-3/#newline
  (unless (boundp '+white-spaces+)
    (defconstant +white-spaces+
      (concatenate 'string +newlines+ '(#\Tab #\Space))))
  ;; https://www.w3.org/TR/css-syntax-3/#non-printable-code-point
  (unless (boundp '+non-printable-code-point+)
    (defconstant +non-printable-code-point+
      (concatenate 'list
                   (loop :for i :upfrom 0 :to 8
                         :collect (code-char i))
                   (string #\Tab)
                   (loop :for i :upfrom #xE :to #x1F
                         :collect (code-char i))
                   (string (code-char #x7F))))))

;;;; PREDICATES

(let ((name-start-code-point
       (uiop:list-to-hash-set (concatenate 'list +letters+ "_"))))
  (defun name-start-code-point-p (char)
    ;; https://www.w3.org/TR/css-syntax-3/#name-start-code-point
    (or (values (gethash char name-start-code-point))
        (non-ascii-code-point-p char))))

(let ((name-code-point
       (uiop:list-to-hash-set (concatenate 'list "-" +digits+))))
  (defun name-code-point-p (char)
    ;; https://www.w3.org/TR/css-syntax-3/#name-code-point
    (or (name-start-code-point-p char)
        (values (gethash char name-code-point)))))

(let ((non-printable-code-point
       (uiop:list-to-hash-set +non-printable-code-point+)))
  (defun non-printable-code-point-p (char)
    (values (gethash char non-printable-code-point))))

(let ((white-spaces (uiop:list-to-hash-set (coerce +white-spaces+ 'list))))
  (defun white-space-p (char) (values (gethash char white-spaces))))

;;;; CONSUMERS
;;;; 4.Tokenization TODO list.
#|
 | * DONE.
 | ? Work in progress.
 | ! Not but actually implemented.
 |
 | [ ] 4.3.1  Consume a token
 | [*] 4.3.2  Consume comments
 | [*] 4.3.3  Consume a numeric token
 | [?] 4.3.4  Consume an ident-like token
 | [*] 4.3.5  Consume a string token
 | [*] 4.3.6  Consume a url token
 | [*] 4.3.7  Consume an escaped code point
 | [*] 4.3.8  Check if two code points are a valid escape
 | [*] 4.3.9  Check if three code points would start an identifier
 | [*] 4.3.10 Check if three code points would start a number
 | [*] 4.3.11 Consume a name
 | [*] 4.3.12 Consume a number
 | [ ] 4.3.13 Convert a string to a number
 | [*] 4.3.14 Consume the remnants of a bad url
 | [ ] 5.4.1  Consume a list of rules
 | [ ] 5.4.2  Consume an at-rule
 | [ ] 5.4.3  Consume a qualified rule
 | [ ] 5.4.4  Consume a list of declarations
 | [ ] 5.4.5  Consume a declaration
 | [ ] 5.4.6  Consume a component value
 | [ ] 5.4.7  Consume a simple block
 | [ ] 5.4.8  Consume a function
 |#

;;;; 4.3.8. Check if two code points are a valid escape
;;; https://www.w3.org/TR/css-syntax-3/#starts-with-a-valid-escape

(defun valid-escape-p (input)
  (let ((c (peek-char nil input nil nil)))
    (cond ((null c) nil) ((find c +newlines+) nil) (t t))))

;;;; 4.3.10. Check if three code points would start a number
;;; https://www.w3.org/TR/css-syntax-3/#starts-with-a-number

(defun start-a-number-p (input)
  (labels ((check-first ()
             (unread-protect (first input)
               (cond ((null first) nil)
                     ((find first "+-") (check-second))
                     ((char= #\. first)
                      (let ((second (peek-char nil input nil nil)))
                        (and second (digit-char-p second 10))))
                     ((digit-char-p first 10))
                     (t nil))))
           (check-second ()
             (unread-protect (second input)
               (cond ((null second) nil)
                     ((digit-char-p second 10))
                     ((char= #\. second)
                      (let ((third (peek-char nil input nil nil)))
                        (and third (digit-char-p third 10))))
                     (t nil)))))
    (check-first)))

;;;; 4.3.7. Consume an escaped code point
;;; https://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point

(defconstant +maximum-allowed-code-point+ #x10FFFF)

(defun surrogatep (code)
  ;; https://infra.spec.whatwg.org/#surrogate
  (<= #xD800 code #xDFFF))

(declaim
 (ftype (function (&optional (or boolean stream)) (values character &optional))
        consume-an-escaped-code-point))

(defun consume-an-escaped-code-point
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  ;; NOTE: escape char is already read.
  (let ((c (read-char input nil nil)))
    (cond ((null c) (internal-logical-error "Must not comes here. ~S" input))
          ((digit-char-p c 16)
           (let ((code
                  (let ((*read-base* 16))
                    (read-from-string
                      (with-output-to-string (*standard-output*)
                        (write-char c)
                        (loop :repeat 5
                              :for c = (read-char input nil nil)
                              :if (null c)
                                :do (loop-finish)
                              :else :if (digit-char-p c 16)
                                :do (write-char c)
                              :else :if (char= #\Space c)
                                :do (loop-finish)
                              :else
                                :do (unread-char c input)
                                    (loop-finish)))))))
             (cond
               ((or (< +maximum-allowed-code-point+ code)
                    (= 0 code)
                    (surrogatep code))
                #.(code-char #xFFFD))
               (t (code-char code)))))
          ((find c +newlines+)
           (internal-logical-error "Must not comes here. ~S" input))
          (t c))))

;;;; 4.3.12. Consume a number
;;; https://www.w3.org/TR/css-syntax-3/#consume-a-number

(declaim
 (ftype (function (&optional (or boolean stream)) (values real &optional))
        consume-a-number))

(defun consume-a-number
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let ((*readtable* (named-readtables:find-readtable :standard)))
    (labels ((sign? ()
               (let ((sign? (read-char input nil nil)))
                 (cond ((null sign?))
                       ((find sign? "+-")
                        (write-char sign?)
                        (digits))
                       (t
                        (unread-char sign? input)
                        (digits)))))
             (non-digit? (c)
               (not (digit-char-p c 10)))
             (digits ()
               (handler-case
                   (core-reader:do-stream-till (c #'non-digit? input)
                     (write-char c))
                 (end-of-file ())))
             (dot? ()
               (let ((dot? (read-char input nil nil)))
                 (cond ((null dot?))
                       ((char= #\. dot?)
                        (write-char dot?)
                        (digits)
                        (exponent?)
                        (sign?))
                       (t
                        (unread-char dot? input)
                        (exponent?)
                        (sign?)))))
             (exponent? ()
               (let ((exponent? (read-char input nil nil)))
                 (cond ((null exponent?))
                       ((find exponent? "eE")
                        (let ((next (read-char input nil nil)))
                          (cond ((null next))
                                ((or (find next "+-") (digit-char-p next 10))
                                 (unread-char next input)
                                 (write-char exponent?)
                                 (sign?))
                                (t
                                 (unread-char next input)
                                 (unread-char exponent? input)))))
                       (t (unread-char exponent? input))))))
      (values (read-from-string
                (with-output-to-string (*standard-output*) (sign?) (dot?)))))))

;;;; 4.3.9. Check if three code points would start an identifier
;;; https://www.w3.org/TR/css-syntax-3/#would-start-an-identifier

(declaim
 (ftype (function (css-input-stream) (values boolean &optional))
        start-an-identifier-p))

(defun start-an-identifier-p (input)
  (labels ((check-first ()
             (unread-protect (first input)
               (cond ((null first) nil)
                     ((char= #\- first) (check-second))
                     ((name-start-code-point-p first) t)
                     ((char= #\\ first) (valid-escape-p input))
                     (t nil))))
           (check-second ()
             (unread-protect (second input)
               (cond ((null second) nil)
                     ((or (name-start-code-point-p second) (char= #\- second))
                      t)
                     ((char= #\\ second) (valid-escape-p input))
                     (t nil)))))
    (check-first)))

;;;; 4.3.11. Consume a name
;;; https://www.w3.org/TR/css-syntax-3/#consume-name

(declaim
 (ftype (function (&optional (or boolean stream))
         (values simple-string &optional))
        consume-a-name))

(defun consume-a-name
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (peek-char t input nil nil)
  (let ((name
         (with-output-to-string (*standard-output*)
           (core-reader:do-stream (c input nil nil)
             (cond ((name-code-point-p c) (write-char c))
                   ((and (char= #\\ c) (valid-escape-p input))
                    (write-char (consume-an-escaped-code-point input)))
                   (t
                    (unread-char c input)
                    (return)))))))
    (if (equal "" name)
        (error 'name-parse-error
               :character (peek-char nil input nil 'end-of-file))
        name)))

;;;; 4.3.3. Consume a numeric token
;;; https://www.w3.org/TR/css-syntax-3/#consume-numeric-token

(defstruct (percentage-token (:include number-token)))

(defstruct (dimension-token (:include number-token))
  (unit (error "UNIT is required.") :type string))

(declaim
 (ftype (function (&optional (or boolean stream))
         (values number-token &optional))
        consume-a-numeric-token))

(defun consume-a-numeric-token
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let ((number (consume-a-number input)) (next (peek-char nil input nil nil)))
    (cond ((null next) (make-number-token :value number))
          ((start-an-identifier-p input)
           (make-dimension-token :value number :unit (consume-a-name input)))
          ((char= #\% next)
           (read-char input) ; discard %.
           (make-percentage-token :value number))
          (t (make-number-token :value number)))))

;;;; 4.3.2. Consume comments
;;; https://www.w3.org/TR/css-syntax-3/#consume-comment

(defun consume-comments
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (handler-case
      (core-reader:do-stream-till-suffix (c "*/" :stream input)
        (declare (ignore c)))
    (end-of-file ()
      (error 'end-of-css :stream input)))
  (values))

;;;; 4.3.14. Consume the remnants of a bad url
;;; https://www.w3.org/TR/css-syntax-3/#consume-the-remnants-of-a-bad-url

(defun consume-the-remnants-of-a-bad-url
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (core-reader:do-stream (c input nil nil)
    (cond ((char= #\) c) (return))
          ((and (char= #\\ c) (valid-escape-p input))
           (consume-an-escaped-code-point input))))
  (values))

;;;; 4.3.6. Consume a url token
;;; https://www.w3.org/TR/css-syntax-3/#consume-url-token

(defstruct (url-token (:include string-token)))

(defstruct (bad-url-token (:include string-token)))

(declaim
 (ftype (function (&optional (or boolean stream))
         (values (or url-token bad-url-token) &optional))
        consume-a-url-token))

(defun consume-a-url-token
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  ;; Note: This algorithm assumes that the initial "url(" has already been consumed.
  ;; This algorithm also assumes that it’s being called to consume an "unquoted" value, like url(foo).
  ;; A quoted value, like url("foo"), is parsed as a <function-token>.
  ;; Consume an ident-like token automatically handles this distinction;
  ;; this algorithm shouldn’t be called directly otherwise.
  (peek-char t input) ; to discard white spaces.
  (let* ((bad-url-p)
         (string
          (with-output-to-string (*standard-output*)
            (core-reader:do-stream (c input nil
                                    (cerror "Finish to consume a url token."
                                            'end-of-css
                                            :stream input))
              (cond ((char= #\) c) (return))
                    ((white-space-p c)
                     (let ((next (peek-char t input nil nil)))
                       (cond
                         ((null next)
                          (cerror "Finish to consume a url token." 'end-of-css
                                  :stream input)
                          (return))
                         ((char= #\) next) (return))
                         (t
                          (consume-the-remnants-of-a-bad-url input)
                          (setf bad-url-p t)
                          (return)))))
                    ((or (find c "\"'(") (non-printable-code-point-p c))
                     (cerror "Finish to consume a url token."
                             'simple-parse-error
                             :format-control "url( immediately follows ~S is invalid."
                             :format-arguments (list c))
                     (consume-the-remnants-of-a-bad-url input)
                     (setf bad-url-p t)
                     (return))
                    ((char= #\\ c)
                     (if (valid-escape-p input)
                         (write-char (consume-an-escaped-code-point input))
                         (progn
                          (cerror
                            "Consume the remnants of a bad url then finish."
                            'invalid-escape
                            :character (peek-char nil input nil 'end-of-file))
                          (consume-the-remnants-of-a-bad-url input)
                          (setf bad-url-p t)
                          (return))))
                    (t (write-char c)))))))
    (if bad-url-p
        (make-bad-url-token :value string)
        (make-url-token :value string))))

;;;; 5.4.7. Consume a simple block
;;; https://www.w3.org/TR/css-syntax-3/#consume-a-simple-block

(declaim
 (ftype (function ((or null character) css-input-stream)
         (values list &optional))
        consume-a-simple-block))

(defun consume-a-simple-block (end-char input)
  ;; Note: This algorithm assumes that the current input token has
  ;; already been read open token e.g. #\( #\[ #\{.
  ;; If end-char is `NIL`, do implicit list behavior.
  (loop :for c = (peek-char t input nil nil)
        :if (null c)
          :do (cerror
                (format nil "Finish to consume a simple block. ~S" end-char)
                'end-of-css
                :stream input)
              (loop-finish)
        :else :if (eql end-char c)
          :do (read-char input) ; discar end-char.
              (loop-finish)
        :else :if (char= #\, c)
          :collect nil ; as null component.
          :and :do (read-char input)
        :else
          :collect (handler-case (read-style input t t t)
                     (name-parse-error (condition)
                       (if (find (parse-error-character condition) "=+-*/")
                           (make-delim-token :value (string (read-char input)))
                           (error condition))))
          :and :do (let ((more? (peek-char t input nil nil)))
                     (cond ((null more?) (loop-finish))
                           ((char= #\, more?) (read-char input)) ; successfully
                                                                 ; discard.
                           ((null end-char) (loop-finish))))))

;;;; 5.4.8. Consume a function
;;; https://www.w3.org/TR/css-syntax-3/#ref-for-typedef-function-token%E2%91%A8

(defstruct (function-token (:include css-token))
  (name (error "NAME is required.") :type string)
  (args (error "VALUE is required.") :type list))

(defun consume-a-function
       (name
        &optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (make-function-token :name name :args (consume-a-simple-block #\) input)))

;;;; 4.3.4. Consume an ident-like token
;;; https://www.w3.org/TR/css-syntax-3/#consume-ident-like-token

(declaim
 (ftype (function (&optional (or boolean stream))
         (values (or url-token bad-url-token function-token simple-string)
                 &optional))
        consume-an-ident-like-token))

(defun consume-an-ident-like-token
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let ((name (consume-a-name input)) (next (read-char input nil nil)))
    (cond
      ((and (equalp "url" name) (eql #\( next))
       (let ((peek (peek-char t input nil nil)))
         (cond ((null peek) (consume-a-url-token input))
               ((find peek "\"'") (consume-a-function name input))
               (t (consume-a-url-token input)))))
      ((eql #\( next) (consume-a-function name input))
      (t
       (when next
         (unread-char next input)
         (when (equal "" name)
           (error 'simple-parse-error
                  :format-control "Could not parse ident like token. ~S"
                  :format-arguments (list next))))
       name))))

;;;; 4.3.5. Consume a string token
;;; https://www.w3.org/TR/css-syntax-3/#consume-string-token

(defstruct (bad-string-token (:include string-token)))

(declaim
 (ftype (function (character &optional (or boolean stream))
         (values string-token &optional))
        consume-a-string-token))

(defun consume-a-string-token
       (delimiter
        &optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let* ((bad-string-p)
         (contents
          (with-output-to-string (*standard-output*)
            (core-reader:do-stream (char input nil
                                         (cerror
                                           "Finish to consume a string token."
                                           'end-of-css
                                           :stream input))
              (cond ((char= char delimiter) (return))
                    ((find char +newlines+)
                     (cerror "Return bad string token." 'simple-parse-error
                             :format-control "~S in the string is invalid."
                             :format-arguments (list char))
                     (unread-char char input)
                     (setf bad-string-p t)
                     (return))
                    ((char= #\\ char)
                     (let ((next (read-char input nil nil)))
                       (cond
                         ((null next) ; do nothing.
                          (cerror "Finish to consume a string token."
                                  'end-of-css
                                  :stream input)
                          (return))
                         ((find next +newlines+)) ; consume newline.
                         (t
                          (unread-char next input)
                          (write-char
                            (consume-an-escaped-code-point input))))))
                    (t (write-char char)))))))
    (if bad-string-p
        (make-bad-string-token :value contents)
        (make-string-token :value contents))))

;;;; 5.4.5. Consume a declaration
;;; https://www.w3.org/TR/css-syntax-3/#consume-declaration

(defstruct css-declaration
  (name (error "NAME is required.") :type string)
  (importantp nil :type boolean)
  (list nil :type list))

(declaim
 (ftype (function (sequence css-input-stream) (values list &optional))
        consume-components))

(defun consume-components (end-chars input)
  ;; NOTE: END-CHAR is not consumed.
  (labels ((style ()
             (handler-case (read-style input t t t)
               (name-parse-error (condition)
                 (let ((c (parse-error-character condition)))
                   (if (find c "+")
                       (make-delim-token :value (string (read-char input)))
                       (error condition))))))
           (decls ()
             (loop :for c = (peek-char t input nil nil)
                   :if (null c)
                     :do (error 'end-of-declaration
                                :stream input
                                :styles styles)
                   :else :if (or (char= #\, c) (find c end-chars))
                     :do (loop-finish)
                   :else
                     :collect (style) :into styles
                   :finally (return styles))))
    (loop :for c := (peek-char t input nil nil)
          :if (null c)
            :do (cerror "Finish to consume components." 'end-of-css
                        :stream input)
                (loop-finish)
          :else :if (find c end-chars)
            :do (loop-finish)
          :else :if (char= #\, c)
            :collect nil ; as null component.
            :and :do (read-char input)
          :else
            :collect (handler-case (decls)
                       (end-of-declaration (c)
                         (cerror "Finish to consume components." c)
                         (styles c)))
            :and :do (let ((c (peek-char t input nil nil)))
                       (cond
                         ((null c)
                          (cerror "Finish to consume components." 'end-of-css
                                  :stream input)
                          (loop-finish))
                         ((find c end-chars) (loop-finish))
                         ((char= #\, c) (read-char input)) ; discard.
                         (t nil))))))

(defstruct (important-token (:include css-token)))

(declaim
 (ftype (function (&optional (or boolean stream))
         (values (or null css-declaration) &optional))
        consume-a-declaration))

(defun consume-a-declaration
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let ((name (read-style input t t t))
        (colon? (peek-char t input nil nil))
        important?)
    (if (not (eql #\: colon?))
        (cerror "Return NIL." 'simple-parse-error
                :format-control "Declaration name does not follow : is invalid. ~S"
                :format-arguments (list colon?))
        (make-css-declaration :name name
                              :list (mapcar
                                      (lambda (x)
                                        (multiple-value-bind (decls imp?)
                                            (remove-if-find #'important-token-p
                                                            x)
                                          (setf important?
                                                  (or important? imp?))
                                          decls))
                                      (progn
                                       (read-char input) ; discard colon.
                                       (consume-components ";}" input)))
                              :importantp important?))))

;;;; 5.4.4. Consume a list of declarations
;;; https://www.w3.org/TR/css-syntax-3/#consume-list-of-declarations

(defun consume-a-list-of-declarations
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (uiop:while-collecting (acc)
    (loop :for c = (peek-char t input nil nil)
          :if (null c)
            :do (cerror "Finish to consume a list of declarations." 'end-of-css
                        :stream input)
                (loop-finish)
          :else :if (char= #\} c)
            :do (read-char input)
                (loop-finish)
          :else :if (char= #\@ c)
            :do (acc (read-style input t t t))
          :else
            :do (handler-case (consume-a-declaration input)
                  (name-parse-error (c)
                    (unless (eql #\} (parse-error-character c))
                      (progn
                       (cerror "Ignore declaration." c)
                       (warn "Discard ~S"
                             (core-reader:read-string-till
                               (lambda (x) (find x ";}")) input)))))
                  (:no-error (decl)
                    (acc decl)))
            :and :do (let ((next (peek-char t input nil nil)))
                       (cond
                         ((null next)
                          (cerror "Finish to consume a list of declarations."
                                  'end-of-css
                                  :stream input)
                          (loop-finish))
                         ((char= #\; next) (read-char input)) ; discard.
                         (t nil))))))

;;;; READERS
;;;; 4.3.1. Consume a token
;;; https://www.w3.org/TR/css-syntax-3/#consume-token

(defstruct (delim-token (:include string-token)))

(declaim (ftype (function (stream character)) |/-reader|))

(defun |/-reader| (input character)
  (let ((next (read-char input nil nil)))
    (cond ((null next) (make-delim-token :value (string character)))
          ((char= #\* next) (consume-comments input))
          (t
           (unread-char next input)
           (make-delim-token :value (string character))))))

(declaim
 (ftype (function (stream character) (values list &optional)) |{-reader|))

(defun |{-reader| (input open-paren)
  (declare (ignore open-paren))
  (consume-a-list-of-declarations input))

(declaim
 (ftype (function (stream character) (values cl-colors2:rgb &optional))
        |#rgb-reader|))

(defun |#rgb-reader| (input hash)
  (declare (ignore hash))
  (let ((hex
         (loop :for c = (read-char input nil nil)
               :repeat 6
               :if (null c)
                 :do (return)
               :else :if (digit-char-p c 16)
                 :collect :it
               :else
                 :do (loop-finish)
               :finally (unread-char c input))))
    (ecase (length hex)
      (3
       (apply #'cl-colors2:rgb
              (mapcar
                (lambda (x) (float (/ (dpb x (byte 4 0) (ash x 4)) #xFF)))
                hex)))
      (6
       (apply #'cl-colors2:rgb
              (loop :for (a b) :on hex :by #'cddr
                    :collect (float (/ (dpb b (byte 4 0) (ash a 4)) #xFF))))))))

(defun |"-reader| (stream character) (consume-a-string-token character stream))

(defstruct qualified-rule
  (selectors nil :type list)
  (declarations nil :type list))

(declaim
 (ftype (function (css-input-stream character)
         (values list ; of-type simple-string.
                 &optional))
        consume-selectors))

(defun consume-selectors (input first-char)
  (let* ((selectors (cons :head nil)) (tail selectors))
    (labels ((consume-selector (thunk)
               (with-output-to-string (*standard-output*)
                 (funcall thunk)
                 (handler-case
                     (core-reader:do-stream-till (c (lambda (c) (find c ",{"))
                                                    input)
                       (write-char c))
                   (end-of-file ()))))
             (trim-whitespaces (string)
               (string-right-trim +white-spaces+ string))
             (collect (x)
               (rplacd tail (setf tail (list x)))))
      (collect
        (trim-whitespaces
          (consume-selector (lambda () (write-char first-char)))))
      (loop (if (not (char= #\, (peek-char t input nil #\Nul)))
                (return)
                (progn
                 (read-char input)
                 (collect
                   (trim-whitespaces (consume-selector (constantly nil)))))))
      (cdr selectors))))

(defun |<-reader| (input character)
  (if (stream-start-with "!--" input)
      (progn
       (dotimes (x 3) (read-char input)) ; discard !--.
       (core-reader:do-stream-till-suffix (c "-->" :stream input)
         (declare (ignore c))))
      (make-delim-token :value (string character))))

(defstruct at-rule
  (name (error "NAME is required.") :type string)
  (components nil :type list)
  (block nil :type list))

(defstruct (at-keyword-token (:include string-token)))

(declaim
 (ftype (function (stream character)
         (values (or at-rule at-keyword-token) &optional))
        |@-reader|))

(defun |@-reader| (input at-sign)
  (cond
    ((start-an-identifier-p input)
     (make-at-rule :name (consume-a-name input)
                   :components (consume-a-simple-block nil input)
                   :block (let ((block? (read-char input nil nil)))
                            (cond
                              ((null block?)
                               (cerror "Use NIL as block." 'end-of-css
                                       :stream input))
                              ((char= #\; block?) nil)
                              ((char= #\{ block?)
                               (consume-a-simple-block #\} input))
                              (t
                               (unread-char block? input)
                               (cerror "Use NIL as block." 'simple-parse-error
                                       :format-control "Missing block."))))))
    (t
     (cerror "Return at keyword token with \"@\"." 'simple-parse-error
             :format-control "Missing at keyword name.")
     (make-at-keyword-token :value (string at-sign)))))

(declaim
 (ftype (function (stream character)
         (values (or delim-token important-token) &optional))
        |!-reader|))

(defun |!-reader| (input !)
  (handler-case (consume-a-name input)
    (name-parse-error (c)
      (cerror "Return delim token." c)
      (make-delim-token :value (string !)))
    (:no-error (name)
      (if (equal "important" name)
          (make-important-token)
          (error 'simple-parse-error
                 :format-control "Unknown syntax !~A."
                 :format-arguments (list name))))))

;;;; CSS-READTABLE
#| https://www.w3.org/TR/css-syntax-3/#parser-diagrams
 |
 | Stylesheet := [ <whitespace-token> | <CDC-token> | <CDO-token> | Qualified-rule | At-rule ]
 | Qualified rule := [ Component-value {} block ]
 | At-rule := [ <at-keyword-token> Component-value [ {}-block | ; ] ]
 |
 | Component value ;= [ Preserved-token | {}-block | ()-block | []-block | Function-block ]
 |
 |#

(named-readtables:defreadtable css-readtable
  (:macro-char #\/ '|/-reader|) ; for comment.
  (:macro-char #\< '|<-reader|) ; for cdc
  (:macro-char #\@ '|@-reader|))

(named-readtables:defreadtable css-non-toplevel
  (:macro-char #\/ '|/-reader|) ; for comment.
  (:macro-char #\" '|"-reader|)
  (:macro-char #\' '|"-reader|)
  (:macro-char #\{ '|{-reader|)
  (:macro-char #\# '|#rgb-reader|)
  (:macro-char #\! '|!-reader|))

;;;; READ-STYLE

(declaim
 (ftype (function (&optional (or boolean stream) boolean t boolean)
         (values t &optional))
        read-style))

(defun read-style
       (&optional (input *standard-input*) (eof-error-p t) eof-value
        recursive-p
        &aux (input (ensure-input-stream input)))
  "Read one css style from specified input."
  (let ((*readtable*
         (named-readtables:find-readtable
           (if recursive-p
               'css-non-toplevel
               'css-readtable))))
    (handler-case (peek-char t input) ; to discard white spaces.
      (end-of-file (c)
        (if eof-error-p
            (error c)
            eof-value))
      (:no-error (char)
        (multiple-value-bind (reader-macro non-terminating-p)
            (get-macro-character char)
          (declare (ignore non-terminating-p))
          (cond
            (reader-macro
             (multiple-value-call
                 (lambda (&rest values)
                   (if values
                       (values-list values)
                       (read-style input eof-error-p eof-value recursive-p)))
               (funcall (coerce reader-macro 'function) input
                        (read-char input #| actually read |#))))
            (recursive-p
             (if (start-a-number-p input)
                 (consume-a-numeric-token input)
                 (consume-an-ident-like-token input)))
            ((or (find char ".#") (start-an-identifier-p input))
             (let ((selectors (consume-selectors input (read-char input))))
               (if (char= #\{ (peek-char t input nil #\Nul))
                   (make-qualified-rule :selectors selectors
                                        :declarations (read-style input t t t))
                   (error 'simple-parse-error
                          :format-control "Missing declarations after ~S."
                          :format-arguments (list selectors)))))
            (t (internal-logical-error "NIY ~S" char))))))))

;;;; READ-STYLE-FROM-STRING

(declaim
 (ftype (function
         (string &optional boolean t &key
                 (:start (mod #.array-total-size-limit))
                 (:end (or null (mod #.array-total-size-limit))))
         (values t (mod #.array-total-size-limit) &optional))
        read-style-from-string))

(locally
 ;; Muffle style warning about
 ;; &OPTINOAL and &KEY found in the same lambda list.
 ;; We design API in a conservative fashion.
 #+sbcl
 (declare (sb-ext:muffle-conditions style-warning))
 (defun read-style-from-string
        (style-string &optional (eof-error-p t) eof-value &key (start 0) end)
   (with-input-from-string (in style-string :start start :end end)
     (let ((in (ensure-input-stream in)))
       (values (read-style in eof-error-p eof-value)
               (+ start (file-position in)))))))

;;;; READ-CSS

(declaim
 (ftype (function (&optional (or boolean stream))
         (values list ; of-type style
                 &optional))
        read-css))

(let ((end-of-file '#:end-of-file))
  (defun read-css
         (&optional (input *standard-input*)
          &aux (input (ensure-input-stream input)))
    "Read all style rules from specified stream."
    (loop :for style = (read-style input nil end-of-file)
          :if (eq style end-of-file)
            :do (loop-finish)
          :else
            :collect style)))
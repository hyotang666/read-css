(in-package :cl-user)

(defpackage :read-css
  (:use :cl)
  (:export))

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

;;;; ABSTRUCT STRUCTURE

(defstruct css-token)

(defstruct (string-token (:include css-token))
  (value (error "VALUE is required.") :type string))

(defstruct (number-token (:include css-token))
  (value (error "VALUE is required.") :type real))

;;;; CONDITIONS

(define-condition css-parse-error (parse-error) ())

(define-condition end-of-css (end-of-file css-parse-error) ())

(define-condition invalid-escape (css-parse-error)
  ((char :initarg :character :reader invalid-char))
  (:report
   (lambda (this output)
     (funcall (formatter "Escape immediately follows ~S is invalid.") output
              (invalid-char this)))))

(define-condition simple-parse-error (css-parse-error simple-condition) ())

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

(defun stream-start-with (string input)
  (labels ((rec (index)
             (if (array-in-bounds-p string index)
                 (let ((char (read-char input nil nil)))
                   (unwind-protect
                       (if (eql char (aref string index))
                           (rec (1+ index))
                           nil)
                     (when char
                       (unread-char char input)))))))
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
  (macrolet ((with-check ((var form) &body body)
               `(let ((,var ,form))
                  (unwind-protect (progn ,@body)
                    (when ,var
                      (unread-char ,var input))))))
    (labels ((check-first ()
               (with-check (first (read-char input nil nil))
                 (cond ((null first) nil)
                       ((find first "+-") (check-second))
                       ((char= #\. first)
                        (let ((second (peek-char nil input nil nil)))
                          (and second (digit-char-p second 10))))
                       ((digit-char-p first 10))
                       (t nil))))
             (check-second ()
               (with-check (second (read-char input nil nil))
                 (cond ((null second) nil)
                       ((digit-char-p second 10))
                       ((char= #\. second)
                        (let ((third (peek-char nil input nil nil)))
                          (and third (digit-char-p third 10))))
                       (t nil)))))
      (check-first))))

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
    (values (read-from-string
              (with-output-to-string (*standard-output*)
                (labels ((sign? ()
                           (let ((sign? (read-char input nil nil)))
                             (cond ((null sign?))
                                   ((find sign? "+-")
                                    (write-char sign?)
                                    (digits))
                                   (t
                                    (unread-char sign? input)
                                    (digits)))))
                         (digits ()
                           (loop :for c = (read-char input nil nil)
                                 :if (null c)
                                   :do (loop-finish)
                                 :else :if (digit-char-p c 10)
                                   :do (write-char c)
                                 :else
                                   :do (unread-char c input)
                                       (loop-finish)))
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
                                            ((or (find next "+-")
                                                 (digit-char-p next 10))
                                             (unread-char next input)
                                             (write-char exponent?)
                                             (sign?))
                                            (t
                                             (unread-char next input)
                                             (unread-char exponent? input)))))
                                   (t (unread-char exponent? input))))))
                  (sign?)
                  (dot?)))))))

;;;; 4.3.9. Check if three code points would start an identifier
;;; https://www.w3.org/TR/css-syntax-3/#would-start-an-identifier

(declaim
 (ftype (function (css-input-stream) (values boolean &optional))
        start-an-identifier-p))

(defun start-an-identifier-p (input)
  (macrolet ((with-cleanup ((var form) &body body)
               `(let ((,var ,form))
                  (unwind-protect (progn ,@body)
                    (when ,var
                      (unread-char ,var input))))))
    (labels ((check-first ()
               (with-cleanup (first (read-char input nil nil))
                 (cond ((null first) nil)
                       ((char= #\- first) (check-second))
                       ((name-start-code-point-p first) t)
                       ((char= #\\ first) (valid-escape-p input))
                       (t nil))))
             (check-second ()
               (with-cleanup (second (read-char input nil nil))
                 (cond ((null second) nil)
                       ((or (name-start-code-point-p second)
                            (char= #\- second))
                        t)
                       ((char= #\\ second) (valid-escape-p input))
                       (t nil)))))
      (check-first))))

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
           (loop :for c = (read-char input nil nil)
                 :if (null c)
                   :do (loop-finish)
                 :else :if (name-code-point-p c)
                   :do (write-char c)
                 :else :if (and (char= #\\ c) (valid-escape-p input))
                   :do (write-char (consume-an-escaped-code-point input))
                 :else
                   :do (unread-char c input)
                       (loop-finish)))))
    (if (equal "" name)
        (error 'simple-parse-error
               :format-control "Could not consume a name. ~S"
               :format-arguments (list input))
        name)))

;;;; 4.3.3. Consume a numeric token
;;; https://www.w3.org/TR/css-syntax-3/#consume-numeric-token

(defstruct (percentage-token (:include number-token)))

(defstruct (dimension-token (:include number-token))
  (type nil :type (member nil :number))
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
  (macrolet ((! (form)
               `(handler-case ,form
                  (end-of-file ()
                    (error 'end-of-css :stream input))
                  (:no-error (char)
                    char))))
    (loop :for char = (! (read-char input))
          :if (char= #\* char)
            :do (let ((end? (! (read-char input))))
                  (when (char= #\/ end?)
                    (return (values)))))))

;;;; 4.3.14. Consume the remnants of a bad url
;;; https://www.w3.org/TR/css-syntax-3/#consume-the-remnants-of-a-bad-url

(defun consume-the-remnants-of-a-bad-url
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (loop :for c = (read-char input nil nil)
        :if (or (null c) (char= #\) c))
          :do (loop-finish)
        :else :if (and (char= #\\ c) (valid-escape-p input))
          :do (consume-an-escaped-code-point input)
              ;; Discar c
              )
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
            (loop :for c = (read-char input nil nil)
                  :if (null c)
                    :do (signal 'end-of-css :stream input)
                        (loop-finish)
                  :else :if (char= #\) c)
                    :do (loop-finish)
                  :else :if (white-space-p c)
                    :do (let ((next (peek-char t input nil nil)))
                          (cond
                            ((null next)
                             (signal 'end-of-css :stream input)
                             (loop-finish))
                            ((char= #\) next) (loop-finish))
                            (t
                             (consume-the-remnants-of-a-bad-url input)
                             (setf bad-url-p t)
                             (loop-finish))))
                  :else :if (or (find c "\"'(") (non-printable-code-point-p c))
                    :do (signal 'simple-parse-error
                                :format-control "url( immediately follows ~S is invalid."
                                :format-arguments (list c))
                        (consume-the-remnants-of-a-bad-url input)
                        (setf bad-url-p t)
                        (loop-finish)
                  :else :if (char= #\\ c)
                    :do (if (valid-escape-p input)
                            (write-char (consume-an-escaped-code-point input))
                            (progn
                             (signal 'invalid-escape
                                     :character (peek-char nil input nil
                                                           'end-of-file))
                             (consume-the-remnants-of-a-bad-url input)
                             (setf bad-url-p t)
                             (loop-finish)))
                  :else
                    :do (write-char c)))))
    (if bad-url-p
        (make-bad-url-token :value string)
        (make-url-token :value string))))

;;;; 4.3.4. Consume an ident-like token
;;; https://www.w3.org/TR/css-syntax-3/#consume-ident-like-token

(defstruct (function-token (:include css-token))
  (name (error "NAME is required.") :type string)
  (args (error "VALUE is required.") :type list))

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
       (character
        &optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let* ((bad-string-p)
         (contents
          (with-output-to-string (*standard-output*)
            (loop :for char = (read-char input nil nil)
                  :if (null char)
                    :do (signal 'end-of-css :stream input)
                        (loop-finish)
                  :else :if (char= char character)
                    :do (loop-finish)
                  :else :if (find char +newlines+)
                    :do (signal 'simple-parse-error
                                :format-control "~S in the string is invalid."
                                :format-arguments (list char))
                        (unread-char char input)
                        (setf bad-string-p t)
                        (loop-finish)
                  :else :if (char= #\\ char)
                    :do (let ((next (read-char input nil nil)))
                          (cond
                            ((null next) ; do nothing.
                             (signal 'end-of-css :stream input))
                            ((find next +newlines+)) ; consume newline.
                            (t
                             (unread-char next input)
                             (write-char
                               (consume-an-escaped-code-point input)))))
                  :else
                    :do (write-char char)))))
    (if bad-string-p
        (make-bad-string-token :value contents)
        (make-string-token :value contents))))

;;;; 5.4.6. Consume a component value
;;; https://www.w3.org/TR/css-syntax-3/#consume-component-value

(defun consume-a-component-value
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (read-style input nil 'end-of-file t))

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
          :do (signal 'end-of-css :stream input)
              (loop-finish)
        :else :if (eql end-char c)
          :do (read-char input) ; discar end-char.
              (loop-finish)
        :else :if (char= #\, c)
          :collect nil ; as null component.
        :else
          :collect (consume-a-component-value input)
          :and :do (let ((more? (peek-char t input nil nil)))
                     (cond ((null more?) (loop-finish))
                           ((char= #\, more?) (read-char input)) ; successfully
                                                                 ; discard.
                           ((null end-char) (loop-finish))))))

;;;; 5.4.8. Consume a function
;;; https://www.w3.org/TR/css-syntax-3/#ref-for-typedef-function-token%E2%91%A8

(defun consume-a-function
       (name
        &optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (make-function-token :name name :args (consume-a-simple-block #\) input)))

;;;; 5.4.5. Consume a declaration
;;; https://www.w3.org/TR/css-syntax-3/#consume-declaration

(defstruct css-declaration
  (name (error "NAME is required.") :type string)
  (importantp nil :type boolean)
  (list nil :type list))

(declaim
 (ftype (function (character css-input-stream) (values list &optional))
        consume-components))

(defun consume-components (end-char input)
  ;; NOTE: END-CHAR is not consumed.
  (loop :for c := (peek-char t input nil nil)
        :if (null c)
          :do (signal 'end-of-css :stream input)
              (loop-finish)
        :else :if (char= end-char c)
          :do (loop-finish)
        :else :if (char= #\, c)
          :collect nil ; as null component.
        :else
          :collect (loop :for c = (peek-char t input nil nil)
                         :if (null c)
                           :do (signal 'end-of-css :stream input)
                               (loop-finish)
                         :else :if (or (char= #\, c) (char= end-char c))
                           :do (loop-finish)
                         :else
                           :collect (consume-a-component-value input))
          :and :do (let ((c (peek-char t input nil nil)))
                     (cond
                       ((null c)
                        (signal 'end-of-css :stream input)
                        (loop-finish))
                       ((char= end-char c) (loop-finish))
                       ((char= #\, c) (read-char input)) ; discard.
                       (t nil)))))

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
        (signal 'simple-parse-error
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
                                       (consume-components #\; input)))
                              :importantp important?))))

;;;; 5.4.4. Consume a list of declarations
;;; https://www.w3.org/TR/css-syntax-3/#consume-list-of-declarations

(defun consume-a-list-of-declarations
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (loop :for c = (peek-char t input nil nil)
        :if (null c)
          :do (signal 'end-of-css :stream input)
              (loop-finish)
        :else :if (char= #\} c)
          :do (read-char input)
              (loop-finish)
        :else :if (char= #\@ c)
          :collect (read-style input t t t)
        :else
          :collect (handler-case (consume-a-declaration input)
                     (parse-error ()
                       (loop-finish)))
          :and :do (let ((next (peek-char t input nil nil)))
                     (cond ((null next) (signal 'end-of-css :stream input))
                           ((char= #\; next) (read-char input)) ; discard.
                           (t nil)))))

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

(defstruct (hash-token (:include string-token)
                       (:constructor make-hash-token
                        (&key value type &aux
                         (type
                          (if type
                              :id
                              :unrestricted)))))
  (type :unrestricted :type (member :unrestricted :id)))

(defstruct qualified-rule
  (selectors nil :type list)
  (declarations nil :type list))

(declaim
 (ftype (function (css-input-stream character)
         (values list ; of-type simple-string.
                 &optional))
        consume-selectors))

(defun consume-selectors (input first-char)
  (uiop:while-collecting (acc)
    (acc
     (string-right-trim +white-spaces+
                        (uiop:strcat first-char
                                     (core-reader:read-string-till
                                       (lambda (c) (find c ",{")) input))))
    (loop (if (not (char= #\, (peek-char t input nil #\Nul)))
              (return)
              (progn
               (read-char input)
               (acc
                (string-right-trim +white-spaces+
                                   (core-reader:read-string-till
                                     (lambda (c) (find c ",{")) input))))))))

(defstruct (cdc-token (:include css-token)))

(defstruct (cdo-token (:include css-token)))

(defun |<-reader| (input character)
  (if (stream-start-with "!--" input)
      (progn
       (dotimes (x 3) (read-char input)) ; discard !--.
       (make-cdo-token))
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
                               (signal 'end-of-css :stream input))
                              ((char= #\; block?) nil)
                              ((char= #\{ block?)
                               (consume-a-simple-block #\} input))
                              (t
                               (unread-char block? input)
                               (signal 'simple-parse-error
                                       :format-control "Missing block."))))))
    (t
     (signal 'simple-parse-error :format-control "Missing at keyword name.")
     (make-at-keyword-token :value (string at-sign)))))

(declaim
 (ftype (function (stream character)
         (values (or delim-token important-token) &optional))
        |!-reader|))

(defun |!-reader| (input !)
  (let ((name (consume-a-name input)))
    (cond
      ((equal "" name)
       (signal 'end-of-css :stream input)
       (make-delim-token :value (string !)))
      ((equal "important" name) (make-important-token))
      (t
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

;;;; READ-CSS

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
            (t (internal-logical-error "NIY"))))))))

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
(in-package :cl-user)

(defpackage :read-css
  (:use :cl)
  (:export))

(in-package :read-css)

;;;; CONDITIONS

(define-condition css-parse-error (end-of-file) ())

;;;; Utilities

(defun ensure-input-stream (stream-designator)
  (etypecase stream-designator
    (stream stream-designator)
    ((eql t) *terminal-io*)
    (null *standard-input*)))

(defun non-ascii-code-point-p (char)
  ;; https://www.w3.org/TR/css-syntax-3/#non-ascii-code-point
  (<= #x80 (char-code char)))



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
      (concatenate 'string +uppercase-letters+ +lowercase-letters+))))

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
                 (string (code-char #x7F)))))

;;;; PREDICATES

(let ((name-start-code-point
       (uiop:list-to-hash-set (concatenate 'list +letters+ "_"))))
  (defun name-start-code-point-p (char)
    ;; https://www.w3.org/TR/css-syntax-3/#name-start-code-point
    (or (non-ascii-code-point-p char)
        (values (gethash char name-start-code-point)))))

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

;;;; CONSUMERS
;;;; 4.3.7. Consume an escaped code point
;;; https://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point
;; Unlike specification, we does not check valid escape before due to
;; difficulty of two or more UNREAD-CHAR.
;; Instead of it, we signals INVALID-ESCAPE.

(define-condition invalid-escape (css-parse-error)
  ((character :initarg :character :reader invalid-escape-character)))

(defconstant +maximum-allowed-code-point+ #x10FFFF)

(defun surrogatep (code)
  ;; https://infra.spec.whatwg.org/#surrogate
  (<= #xD800 code #xDFFF))

(defun consume-an-escaped-code-point
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  ;; NOTE: escape char is already read.
  (let ((c (read-char input nil nil)))
    (cond
      ((null c)
       (signal 'css-parse-error :stream input)
       #.(code-char #xFFFD))
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
      ((find c +newlines+) (error 'invalid-escape :stream input :character c))
      (t c))))

;;;; 4.3.12. Consume a number
;;; https://www.w3.org/TR/css-syntax-3/#consume-a-number

(defun consume-a-number
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (values (read-from-string
            (with-output-to-string (*standard-output*)
              (labels ((next? (string char)
                         (if (find char string)
                             (progn (write-char char) (read-char input))
                             char))
                       (consume-digits (char)
                         (if (and char (digit-char-p char 10))
                             (progn
                              (write-char char)
                              (consume-digits (read-char input nil nil)))
                             char)))
                (let ((next
                       (consume-digits
                         (next? "+-"
                                (next? "eE"
                                       (consume-digits
                                         (next? "."
                                                (consume-digits
                                                  (next? "+-"
                                                         (read-char
                                                           input))))))))))
                  (when (characterp next)
                    (unread-char next input))))))))

;;;; 4.3.11. Consume a name
;;; https://www.w3.org/TR/css-syntax-3/#consume-name

(defun consume-a-name
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (with-output-to-string (*standard-output*)
    (loop :for c = (read-char input nil nil)
          :if (null c)
            :do (loop-finish)
          :else :if (name-code-point-p c)
            :do (write-char c)
          :else :if (char= #\\ c)
            :do (write-char (consume-an-escaped-code-point input))
          :else
            :do (unread-char c input)
                (loop-finish))))

;;;; 4.3.3. Consume a numeric token
;;; https://www.w3.org/TR/css-syntax-3/#consume-numeric-token

(defstruct percentage-token (value (error "VALUE is required.") :type real))

(defstruct dimension-token
  (value (error "VALUE is required.") :type real)
  (type nil :type (member nil :number))
  (unit (error "UNIT is required.") :type string))

(defun consume-a-numeric-token
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let ((number (consume-a-number input)) (next (peek-char nil input nil nil)))
    (cond ((null next) number)
          ((or (char= #\- next)
               (name-start-code-point-p next)
               (char= #\\ next))
           (make-dimension-token :type :number
                                 :value number
                                 :unit (consume-a-name input)))
          ((char= #\% next)
           (read-char input) ; discard %
           (make-percentage-token :value number))
          (t number))))

;;;; 4.3.2. Consume comments
;;; https://www.w3.org/TR/css-syntax-3/#consume-comment

(defun consume-comments
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (macrolet ((! (form)
               `(handler-case ,form
                  (end-of-file ()
                    (error 'css-parse-error :stream input))
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
        :else :if (char= #\\ c)
          :do (handler-case (consume-an-escaped-code-point input)
                (css-parse-error ()))
              ;; Discar c
              )
  (values))

;;;; 4.3.6. Consume a url token
;;; https://www.w3.org/TR/css-syntax-3/#consume-url-token

(defstruct url-token (value (error "VALUE is required.") :type string))

(defstruct bad-url-token (value (error "VALUE is required.") :type string))

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
                    :do (signal 'css-parse-error :stream input)
                        (loop-finish)
                  :else :if (char= #\) c)
                    :do (loop-finish)
                  :else :if (find c +white-spaces+)
                    :do (let ((next (peek-char t input nil nil)))
                          (cond
                            ((null next)
                             (signal 'css-parse-error :stream input)
                             (loop-finish))
                            ((char= #\) next) (loop-finish))
                            (t
                             (consume-the-remnants-of-a-bad-url input)
                             (setf bad-url-p t)
                             (loop-finish))))
                  :else :if (or (find c "\"'(") (non-printable-code-point-p c))
                    :do (signal 'css-parse-error :stream input)
                        (consume-the-remnants-of-a-bad-url input)
                        (setf bad-url-p t)
                        (loop-finish)
                  :else :if (char= #\\ c)
                    :do (handler-case (consume-an-escaped-code-point input)
                          (css-parse-error (c)
                            (signal c)
                            (consume-the-remnants-of-a-bad-url input)
                            (setf bad-url-p t)
                            (loop-finish))
                          (:no-error (char)
                            (write-char char)))
                  :else
                    :do (write-char c)))))
    (if bad-url-p
        (make-bad-url-token :value string)
        (make-url-token :value string))))

;;;; 4.3.5. Consume a string token
;;; https://www.w3.org/TR/css-syntax-3/#consume-string-token

(defstruct string-token (value (error "VALUE is required.") :type string))

(defstruct bad-string-token (value (error "VALUE is required.") :type string))

(defun consume-a-string-token
       (character
        &optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let* ((bad-string-p)
         (contents
          (with-output-to-string (*standard-output*)
            (loop :for char = (read-char input nil nil)
                  :if (null char)
                    :do (signal 'css-parse-error :stream input)
                        (loop-finish)
                  :else :if (char= char character)
                    :do (loop-finish)
                  :else :if (find char +newlines+)
                    :do (signal 'css-parse-error :stream input)
                        (unread-char char input)
                        (setf bad-string-p t)
                        (loop-finish)
                  :else :if (char= #\\ char)
                    :do (handler-case (consume-an-escaped-code-point input)
                          (css-parse-error ()) ; do-nothing.
                          (:no-error (c)
                            (write-char c)))
                  :else
                    :do (write-char char)))))
    (if bad-string-p
        (make-bad-string-token :value contents)
        (make-string-token :value contents))))

;;;; READERS

(defun |/*-reader| (stream character number)
  (declare (ignore character number))
  (consume-comments stream))

(defun |"-reader| (stream character) (consume-a-string-token character stream))

;;;; CSS-READTABLE

(named-readtables:defreadtable css-readtable
  (:macro-char #\/ :dispatch)
  (:dispatch-macro-char #\/ #\* '|/*-reader|)
  (:macro-char #\" '|"-reader|)
  (:macro-char #\' '|"-reader|))

;;;; READ-CSS

(defun read-style
       (&optional (input *standard-input*) (eof-error-p t) eof-value
        &aux (input (ensure-input-stream input)))
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
           (funcall (coerce reader-macro 'function) input
                    (read-char input #| actually read |#)))
          ((or (find char "+-") (digit-char-p char 10))
           (consume-a-numeric-token input))
          (t (read input eof-error-p eof-value)))))))

(declaim
 (ftype (function (&optional (or boolean stream))
         (values list ; of-type style
                 &optional))
        read-css))

(let ((end-of-file '#:end-of-file))
  (defun read-css
         (&optional (input *standard-input*)
          &aux (input (ensure-input-stream input)))
    (let ((*readtable* (named-readtables:find-readtable 'css-readtable)))
      (uiop:while-collecting (acc)
        (loop (multiple-value-call
                  (lambda (&rest styles)
                    (typecase styles
                      (null) ; Zero values.
                      ((cons * null) ; One value.
                       (if (eq end-of-file (car styles))
                           (return)
                           (acc (car styles))))
                      (otherwise ; Some values
                       (error "NIY"))))
                (read-style input nil end-of-file)))))))


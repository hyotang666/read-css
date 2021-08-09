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

;;;; CONSUMERS
;;;; 4.3.7. Consume an escaped code point
;;; https://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point

(defconstant +maximum-allowed-code-point+ #x10FFFF)

(defun surrogatep (code)
  ;; https://infra.spec.whatwg.org/#surrogate
  (<= #xD800 code #xDFFF))

(defun consume-an-escaped-code-point
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (let ((code
         (let ((*read-base* 16))
           (read-from-string
             (with-output-to-string (*standard-output*)
               (loop :repeat 6
                     :for c = (read-char input)
                     :if (digit-char-p c 16)
                       :do (write-char c)
                     :else :if (char= #\Space c)
                       :do (loop-finish)))))))
    (cond
      ((or (< +maximum-allowed-code-point+ code) (= 0 code) (surrogatep code))
       (code-char #xFFFD))
      (t (code-char code)))))

;;;; 4.3.12. Consume a number
;;; https://www.w3.org/TR/css-syntax-3/#consume-a-number

(defun consume-a-number
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (read input))

;;;; 4.3.11. Consume a name
;;; https://www.w3.org/TR/css-syntax-3/#consume-name

(defun consume-a-name (&optional stream eof-errorp eof-value)
  (with-output-to-string (*standard-output*)
    (loop :for c = (read-char stream nil nil)
          :if (null c)
            :do (if eof-errorp
                    (error 'css-parse-error :stream stream)
                    (return-from consume-a-name eof-value))
          :else :if (name-code-point-p c)
            :do (write-char c)
          :else :if (char= #\\ c)
            :do (write-char (consume-an-escaped-code-point input))
          :else
            :do (loop-finish))))

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
  (let ((number (consume-a-number input)) (next (read-char input nil nil)))
    (cond ((null next) number)
          ((or (char= #\- next)
               (name-start-code-point-p next)
               (char= #\\ next))
           (unread-char next)
           (make-dimension-token :type :number
                                 :value number
                                 :unit (consume-a-name input nil "")))
          ((char= #\% next) (make-percentage-token :value number))
          (t number))))

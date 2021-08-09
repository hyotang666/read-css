(in-package :cl-user)

(defpackage :read-css
  (:use :cl)
  (:export))

(in-package :read-css)

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

;;;; 4.3.12. Consume a number
;;; https://www.w3.org/TR/css-syntax-3/#consume-a-number

(defun consume-a-number
       (&optional (input *standard-input*)
        &aux (input (ensure-input-stream input)))
  (read input))

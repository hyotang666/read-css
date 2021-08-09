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

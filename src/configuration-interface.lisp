;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LLA -*-
;;; Copyright (c) 2023 Symbolics Pte. Ltd. All rights reserved.
(in-package #:lla)

;;; ********************************************************************
;;; See the documentation on how to configure libraries.  It is highly
;;; unlikely that you need to change anything in this file.
;;; ********************************************************************

(defun query-configuration (indicator &optional default)
  "Return the property for INDICATOR from the configuration variable, with an optional default, which can be a function (called on demand)."
  (let* ((symbol 'cl-user::*lla-configuration*)
         (plist (when (boundp symbol)
                  (symbol-value symbol)))
         (unique (gensym))
         (configuration (getf plist indicator unique)))
    (if (eq configuration unique)       ; only call when necessary
        (if (functionp default)
            (funcall default)
            default)
        configuration)))

(defun set-feature (symbol set?)
  "Ensure that symbol is in *FEATURES* iff SET?.  Returns no values."
  (if set?
      (pushnew symbol *features*)
      (alexandria:removef *features* symbol))
  (values))

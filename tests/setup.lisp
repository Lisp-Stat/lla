;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
(uiop:define-package #:lla-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils
        #:num-utils.matrix-shorthand
        #:select
        #:clunit
        #:cffi
        #:let-plus
        #:lla)
  (:shadowing-import-from #:alexandria #:mean #:variance #:median)
  (:shadowing-import-from #:lla #:invert) ; also in SELECT
  (:export #:run))

(in-package :lla-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all the tests for LLA."
  (run-suite 'tests :use-debugger interactive?))

;; support functions

(defun array= (array1 array2)
  "Test that arrays are equal and have the same element type."
  (and (type= (array-element-type array1)
              (array-element-type array2))
       (equalp array1 array2)))

(defun random-array (type &rest dimensions)
  "Random array for testing."
  (aops:generate* type
                  (if (subtypep type 'complex)
                      (lambda () (coerce (complex (random 100)
                                                  (random 100))
                                         type))
                      (lambda () (coerce (random 100) type)))
                  dimensions))

(defmacro with-foreign-temporary-buffer ((pointer size) &body body)
  "Allocate a buffer for SIZE complex doubles."
  `(with-foreign-pointer (,pointer (* ,size (foreign-type-size :double) 2))
     ,@body))

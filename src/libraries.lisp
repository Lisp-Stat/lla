;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LLA -*-
;;; Copyright (c) 2023 Symbolics Pte. Ltd. All rights reserved.
(in-package #:lla)

;;; ********************************************************************
;;; See the documentation on how to configure libraries.  It is highly
;;; unlikely that you need to change anything in this file.
;;; ********************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (lib (query-configuration :libraries #'default-libraries))
    (load-foreign-library lib)))

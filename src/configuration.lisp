;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LLA -*-
;;; Copyright Tamas Papp 2010-2011.
;;; Copyright (c) 2023,2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:lla)

;;; ********************************************************************
;;; See the documentation on how to configure libraries.  It is highly
;;; unlikely that you need to change anything in this file.
;;; ********************************************************************

(defun default-libraries ()
  "Return a list of libraries.  The source conditions on the platform, relying TRIVIAL-FEATURES.  This function is only called when the libraries were not configured by the user, see the documentation on how to do that."
  #+linux '((:or "libblas.so.3gf" "libblas.so")
            (:or "liblapack.so.3gf" "liblapack.so"))
  #+windows '("libopenblas.dll")
  #+darwin '("libblas.dylib" "liblapack.dylib")
  #-(or linux windows darwin) '((:default "libblas") (:default "liblapack")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-feature 'lla::int64 (query-configuration :int64 nil))
  (set-feature 'lla::cffi-pinning
               (query-configuration :cffi-pinning #+(or sbcl cll) nil
                                                  #-(or sbcl ccl) t))
  (set-feature 'lla::debug (query-configuration :debug nil))
  (let+ ((efficiency-warnings (query-configuration :efficiency-warnings nil))
         ((&flet enable-efficiency-warning (feature keyword)
            (set-feature feature (find keyword efficiency-warnings)))))
    (enable-efficiency-warning 'lla::efficiency-warning-array-type
                               :array-type)
    (enable-efficiency-warning 'lla::efficiency-warning-array-conversion
                               :array-conversion)))


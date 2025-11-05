;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright Tamas Papp 2010-2011.
;;; Copyright (c) 2023-2025 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defsystem "lla"
  :description "Lisp Linear Algebra"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :version "0.5.1"
  :author "Steven Nunez"
  :license :ML-PL
  :depends-on (#:anaphora
               #:alexandria
               #:cffi
               #:num-utils
               #:select
               #:let-plus)
  :in-order-to ((test-op (test-op "lla/tests")))
  :pathname #P"src/"
  :serial t
  :components
  ((:file "package")
   (:file "configuration-interface")
   (:file "configuration")
   (:file "libraries")
   (:file "conditions")
   (:file "types")
   (:file "foreign-memory")
   (:file "pinned-array")
   (:file "factorizations")
   (:file "fortran-call")
   (:file "linear-algebra")
   (:file "blas")))

(defsystem "lla/tests"
  :description "Unit tests for LLA."
  :author "Steven Nunez"
  :license :MS-PL
  :depends-on (#:lla
               #:clunit2
	       #:select)
  :pathname #P"tests/"
  :serial t
  :components
  ((:file "setup")
   (:file "pinned-array")
   (:file "linear-algebra")
   (:file "blas"))
  :perform (test-op (o s)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (symbol-call :clunit :run-suite
				   (find-symbol* :tests
						 :lla-tests)
					   :use-debugger nil))))

(defmethod perform :after
  ((operation load-op) (system (eql (find-system :lla))))
  "Update *FEATURES* if the system loads successfully."
  (pushnew :lla common-lisp:*features*))

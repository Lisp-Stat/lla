;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER -*-
;;; Copyright (c) 2023 Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:lla
  (:use #:common-lisp
        #:alexandria
        #:anaphora
        #:cffi
        #:num-utils
        #:select
        #:let-plus)
  (:shadow #:invert)                     ; also in SELECT

  ;; no exports from:
  ;;   configuration-interface
  ;;   configuration
  ;;   libraries
  ;;   foreign-memory
  ;;   pinned-array
  (:export                              ; types
   #:lla-integer
   #:lla-single
   #:lla-double
   #:lla-complex-single
   #:lla-complex-double)
  (:export                              ; conditions
   #:lla-internal-error
   #:lla-unhandled-type
   #:lapack-error
   #:lapack-invalid-argument
   #:lapack-failure
   #:lapack-singular-matrix
   #:lla-incompatible-dimensions
   #:lla-efficiency-warning
   #:*lla-efficiency-warning-array-type*
   #:lla-efficiency-warning-array-type
   #:*lla-efficiency-warning-array-conversion*
   #:lla-efficiency-warning-array-conversion)
  (:export                              ; factorizations
   #:lu
   #:lu-u
   #:lu-l
   #:ipiv
   #:ipiv-inverse
   #:qr
   #:qr-r
   #:matrix-square-root
   #:xx
   #:left-square-root
   #:right-square-root
   #:cholesky
   #:hermitian-factorization
   #:spectral-factorization
   #:spectral-factorization-w
   #:spectral-factorization-z
   #:svd
   #:svd-u
   #:svd-d
   #:svd-vt)
  (:export                              ; fortran-call
   #:with-fp-traps-masked)
  (:export                              ; linear-algebra
   #:mm
   #:mmm
   #:outer
   #:solve
   #:invert
   #:least-squares
   #:invert-xx
   #:eigenvalues
   #:logdet
   #:det
   #:tr)
  (:export                              ; blas
   #:gemm!
   #:scal!
   #:axpy!
   #:copy!
   #:dot
   #:asum
   #:nrm2))

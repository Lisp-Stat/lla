;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: LLA -*-
;;; Copyright (c) 2023 Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:lla)

;;; pivoting mixin

(defclass ipiv-mixin ()
  ((ipiv-internal
    :documentation "pivot indices in LAPACK's representation, counting from 1"
    :type vector
    :initarg :ipiv-internal
    :reader ipiv-internal))
  (:documentation "Mixin class for objects with pivoting."))

(defun ipiv (object)
  "Pivot indices, counting from 0, in a format understood by SELECT.
Example:

   (let+ (((&accessors-r/o lu-l lu-u ipiv) (lu a)))
     (num= (select a ipiv) (mm lu-l lu-u) ipiv-inverse t))    ; => T
"
  (let* ((ipiv-internal (ipiv-internal object))
         (length (length ipiv-internal)))
    (aprog1 (ivec length)
      (loop for index below length
            do (let ((pivot-index (1- (aref ipiv-internal index))))
                 (unless (= index pivot-index)
                   (rotatef (aref it index) (aref it pivot-index))))))))

(defun ipiv-inverse (object)
  "Inverted permutation for pivot indices, in a format understood by SELECT.

Example:

   (let+ (((&accessors-r/o lu-l lu-u ipiv-inverse) (lu a)))
     (num= a (select (mm lu-l lu-u) ipiv-inverse t)))        ; => T
"
  (let* ((ipiv (ipiv object))
         (n (length ipiv)))
    (aprog1 (make-array n :element-type 'fixnum)
      (loop for index from 0
            for p across ipiv
            do (setf (aref it p) index)))))

;;;; LU factorization

(defclass lu (ipiv-mixin)
  ((lu
    :documentation "matrix storing the transpose of the LU factorization."
    :initarg :lu
    :reader lu-matrix))
  (:documentation "LU factorization of a matrix with pivoting.  (SELECT A IPIV) is (MM L U), when IPIV is used to obtain the permutation."))

(defun lu-u (lu)
  "Return the U part of an LU factorization."
  (upper-triangular-matrix (lu-matrix lu)))

(defun lu-l (lu)
  "Return the L part of an LU factorization."
  (aprog1 (lower-triangular-matrix (copy-array (lu-matrix lu)))
    (let+ (((&structure-r/o wrapped-matrix- elements) it)
           ((nrow ncol) (array-dimensions elements))
           (one (coerce 1 (array-element-type elements))))
      (dotimes (index (min nrow ncol))
        (setf (aref elements index index) one)))))

(defmethod print-object ((lu lu) stream)
  (print-unreadable-object (lu stream :type t)
    (format stream "~2& L=~A~2& U=~A~2&  pivot indices=~A"
            (lu-l lu)
            (lu-u lu)
            (ipiv-internal lu))))

;;;; QR factorization

(defclass qr ()
  ((qr :initarg :qr
       :reader qr
       :documentation "matrix storing the QR factorization.")
   (tau :accessor tau
        :initarg :tau
        :documentation "complex scalar for elementary reflectors (see documentation of xGEQRF)."))
  (:documentation "QR factorization of a matrix."))

(defun qr-r (qr)
  "Return the R part of a QR factorization."
  (let+ (((&slots-r/o qr) qr)
         ((aops:&dims nrow ncol) qr))
    (upper-triangular-matrix (copy-array (aops:partition qr 0 (min nrow ncol))))))

;;;; generic interface for square root-like factorizations

(defstruct (matrix-square-root (:constructor make-matrix-square-root (left)))
  "General class for representing XX^T decompositions of matrices, regardless of how they were computed.  The convention is to store X, the left square root."
  left)

(defgeneric left-square-root (a)
  (:documentation "Return X such that XX^T=A.")
  (:method ((a matrix-square-root))
    (matrix-square-root-left a)))

(defgeneric right-square-root (a)
  (:documentation "Return Y such that Y^T Y=A.

Efficiency note: decompositions should store the left square root X, and compute Y=X^T on demand, so getting X directly might be more efficient if you don't need X^T.")
  (:method ((a matrix-square-root))
    (transpose (matrix-square-root-left a)))
  (:method (a)
    (transpose (left-square-root a))))

(declaim (inline xx))
(defun xx (left-square-root)
  "Convenience function to create a matrix from a left square root."
  (make-matrix-square-root left-square-root))

(defmacro define-matrix-square-root-scalar-multiplication
    (type &key (make (symbolicate '#:make- type)))
  `(progn
     (defmethod e2* ((a ,type) (b number))
       (,make (e2* (left-square-root a) (sqrt b))))
     (defmethod e2* ((a number) (b ,type))
       (,make (e2* (sqrt a) (left-square-root b))))
     (defmethod e2/ ((a ,type) (b number))
       (,make (e2/ (left-square-root a) (sqrt b))))))

(define-matrix-square-root-scalar-multiplication matrix-square-root)

;;;; Cholesky factorization

(defstruct (cholesky (:include matrix-square-root)
                     (:constructor make-cholesky% (left)))
  "Cholesky factorization a matrix.")

(defun make-cholesky (left)
  (assert (typep left '(and lower-triangular-matrix (satisfies aops:square-matrix?))))
  (make-cholesky% left))

;;;; permutations (pivoting)

(defgeneric permutations (object)
  (:documentation "Return the number of permutations in object (which is usually a matrix factorization, or a pivot index."))

(defun count-permutations% (ipiv-internal)
  "Count the permutations in a pivoting vector."
  (loop
    for index from 1               ; LAPACK counts from 1
    for i across ipiv-internal
    counting (/= index i)))

(defmethod permutations ((lu lu))
  (count-permutations% (ipiv-internal lu)))

;;;; hermitian factorization

(defclass hermitian-factorization (ipiv-mixin)
  ((factor :initarg :factor :reader factor
           :documentation "see documentation of *SYTRF and *HETRF, storage is
           in the half specified by HERMITIAN-ORIENTATION and otherwise
           treated as opaque."))
  (:documentation "Factorization for an indefinite hermitian matrix with
  pivoting."))

;;;; spectral factorization

(defstruct spectral-factorization
  "Z W Z^T factorization of a Hermitian matrix, where the columns of Z contain the eigenvectors and W is a diagonal matrix of the eigenvalues.  Z is a unitary matrix."
  z w)

;;;; svd

(defstruct svd
  "Singular value decomposition.  Singular values are in S, in descending order.  U and VT may be NIL in case they are not computed."
  (u nil) d (vt nil))

;;;; elementwise operations for factorizations

(defmacro define-factorization-eops% (type conversion)
  "Define elementwise operations for TYPE, trying to convert into arrays."
  (check-types (type conversion) symbol)
  `(progn
     (defmethod e2+ ((a ,type) b) (e2+ (,conversion a) b))
     (defmethod e2+ (a (b ,type)) (e2+ a (,conversion b)))))

(define-factorization-eops% matrix-square-root aops:as-array)

;;;; Copyright 2011 Google Inc.  All Rights Reserved

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;     * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;     * Neither the name of Google Inc. nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Author: brown@google.com (Robert Brown)

;;;; Floating point encoding and decoding for Lispworks.

(in-package #:lispworks-float)
(declaim #.*optimize-default*)

(declaim (ftype (function (single-float) (values (signed-byte 32) &optional)) single-float-bits))

(defun single-float-bits (x)
  (declare (type single-float x))
  ;; TODO(brown): Implement using Lispworks FLI functions.
  (portable-float:single-float-bits x))

(declaim (ftype (function (double-float) (values (signed-byte 64) &optional)) double-float-bits))

(defun double-float-bits (x)
  (declare (type double-float x))
  ;; TODO(brown): Implement using Lispworks FLI functions.
  (portable-float:double-float-bits x))

(declaim (ftype (function ((signed-byte 32)) (values single-float &optional)) make-single-float))

(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (fli:with-dynamic-foreign-objects ((value :lisp-single-float))
    (fli:with-coerced-pointer (pointer :type :uint32) value
      (setf (fli:dereference pointer) bits))
    (fli:dereference value)))

(declaim (ftype (function ((signed-byte 32) (unsigned-byte 32)) (values double-float &optional))
                make-double-float))

(defun make-double-float (high-bits low-bits)
  (declare (type (signed-byte 32) high-bits)
           (type (unsigned-byte 32) low-bits))
  (fli:with-dynamic-foreign-objects ((value :lisp-double-float))
    (fli:with-coerced-pointer (pointer :type :uint32) value
      ;; TODO(brown): Use the pointer type :uint64 above and remove the endian conditionals.
      #+little-endian
      (progn (setf (fli:dereference pointer :index 0) low-bits)
             (setf (fli:dereference pointer :index 1) high-bits))
      #-little-endian
      (progn (setf (fli:dereference pointer :index 1) high-bits)
             (setf (fli:dereference pointer :index 0) low-bits)))
    (fli:dereference value)))

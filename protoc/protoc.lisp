;;;; Copyright 2012 Google Inc.  All Rights Reserved

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

;;;; Protocol buffer compiler Common Lisp plugin.

(in-package #:protoc)
(declaim #.*optimize-default*)

(defparameter *plugin-input* "unittest-plugin-input")

(defun read-plugin-input ()
  (with-open-file (input *plugin-input* :direction :input :element-type 'octet)
    (let* ((size (file-length input))
           (buffer (make-octet-vector size))
           (code-generator-request (make-instance 'code-generator-request)))
      (read-sequence buffer input)
      (pb:merge-from-array code-generator-request buffer 0 size)
      code-generator-request)))

(defparameter *parsed-proto* "unittest_import.pb")

(defun read-file-descriptor-set ()
  (with-open-file (input *parsed-proto* :direction :input :element-type 'octet)
    (let* ((size (file-length input))
           (buffer (make-octet-vector size))
           (file-descriptor-set (make-instance 'file-descriptor-set)))
      (read-sequence buffer input)
      (pb:merge-from-array file-descriptor-set buffer 0 size)
      file-descriptor-set)))

(defun hyphenate-studly-caps (string)
  (setf string (cl-ppcre:regex-replace "([A-Z]+)([A-Z][a-z])" string "\\1-\\2"))
  (setf string (cl-ppcre:regex-replace "([a-z0-9])([A-Z])" string "\\1-\\2"))
  (setf string (cl-ppcre:regex-replace "([A-Za-z])([0-9])" string "\\1-\\2"))
  string)

(defun lispify-name (name)
  (let ((hyphenated (hyphenate-studly-caps name)))
    (intern (string-upcase (substitute #\- #\_ hyphenated)))))

(defun class-symbol (descriptor)
  (lispify-name (pb:string-value (name descriptor))))

(defun slot-definition (descriptor)
  (declare (ignore descriptor))
  '())

(defun message-defclass (descriptor)
  (let ((class-symbol (class-symbol descriptor))
        (field-count (length (field descriptor)))
        (slot-definitions (loop for field across (field descriptor)
                                collect (slot-definition field))))
    `((defclass ,class-symbol (pb:protocol-buffer)
        (,@slot-definitions
         (%has-bits% :accessor %has-bits% :initform 0 :type (unsigned-byte ,field-count))
         (pb::%cached-size% :initform 0 :type vector-index)))
      (export ',class-symbol))))

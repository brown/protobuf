;;;; Copyright 2014 Google Inc.  All Rights Reserved

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
  (setf string (cl-ppcre:regex-replace "([a-z])([A-Z])" string "\\1-\\2"))
  (setf string (cl-ppcre:regex-replace "([0-9])([A-Za-z])" string "\\1-\\2"))
  (setf string (cl-ppcre:regex-replace "([A-Za-z])([0-9])" string "\\1-\\2"))
  string)

(defun lispify-name (descriptor)
  (string-upcase (substitute #\- #\_ (hyphenate-studly-caps (pb:string-value (name descriptor))))))

(defun name-symbol (descriptor)
  (intern (lispify-name descriptor)))

(defun slot-definition (descriptor)
  (declare (ignore descriptor))
  '())

;; (defun message-defclass (descriptor)
;;   (let ((class-symbol (class-symbol descriptor))
;;         (field-count (length (field descriptor)))
;;         (slot-definitions (loop for field across (field descriptor)
;;                                 collect (slot-definition field))))
;;     `((defclass ,class-symbol (pb:protocol-buffer)
;;         (,@slot-definitions
;;          (%has-bits% :accessor %has-bits% :initform 0 :type (unsigned-byte ,field-count))
;;          (pb::%cached-size% :initform 0 :type vector-index)))
;;       (export ',class-symbol))))


;;; ========================================
#|
(defvar *input* (read-plugin-input))
(defvar *enum* (aref (enum-type (aref (proto-file *input*) 0)) 0))
(generate-enum *enum*)
|#

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(defun pb-intern (name)
  (intern name (find-package 'protocol-buffer)))

(defun generate-enum (enum-descriptor)
  (let ((enum-name (lispify-name enum-descriptor))
        (values (value enum-descriptor)))
    (labels ((constant-symbol (prefix suffix)
               (pb-intern (concat "+" prefix "-" suffix "+")))
             (enum-constant-symbol (enum-value-descriptor)
               (constant-symbol enum-name (lispify-name enum-value-descriptor))))
      (let* ((constant-definitions
               (loop for value across values
                     collect `(defconstant ,(enum-constant-symbol value) ,(number value))))
             (min-integer (loop for value across values minimize (number value)))
             (max-integer (loop for value across values maximize (number value)))
             (min-symbol (constant-symbol "MINIMUM" enum-name))
             (max-symbol (constant-symbol "MAXIMUM" enum-name))
             (exports (append (loop for value across values collect (enum-constant-symbol value))
                              (list min-symbol max-symbol))))
        `(progn
           (deftype ,(pb-intern enum-name) () '(signed-byte 32))
           ,@constant-definitions
           (defconstant ,min-symbol ,min-integer)
           (defconstant ,max-symbol ,max-integer)
           (export ',exports))))))

;;; ========================================

#|
(defvar *message* (aref (message-type (aref (proto-file *input*) 0)) 0))
|#


(defun field-type (field-descriptor-proto-type)
  (ecase field-descriptor-proto-type
    (+field-descriptor-proto-type-type-bool+ 'boolean)

    ((+field-descriptor-proto-type-type-int32+
      +field-descriptor-proto-type-type-sfixed32+
      +field-descriptor-proto-type-type-sint32+
      +field-descriptor-proto-type-type-enum+) '(signed-byte 32))

    ((+field-descriptor-proto-type-type-uint32+
      +field-descriptor-proto-type-type-fixed32+) '(unsigned-byte 32))

    ((+field-descriptor-proto-type-type-int64+
      +field-descriptor-proto-type-type-sfixed64+
      +field-descriptor-proto-type-type-sint64+) '(signed-byte 64))

    ((+field-descriptor-proto-type-type-uint64+
      +field-descriptor-proto-type-type-fixed64+) '(unsigned-byte 64))

    (+field-descriptor-proto-type-type-double+ 'double-float)
    (+field-descriptor-proto-type-type-float+ 'single-float)

    (+field-descriptor-proto-type-type-string+ 'pb::%sf%)
    (+field-descriptor-proto-type-type-bytes+ '(simple-array (unsigned-byte 8) (*)))

    ((+field-descriptor-proto-type-type-group+
      +field-descriptor-proto-type-type-message+) t)))

(defun field-wire-size (field-descriptor-proto-type form)
  (ecase field-descriptor-proto-type
    (+field-descriptor-proto-type-type-bool+ 1)

    ((+field-descriptor-proto-type-type-fixed32+
      +field-descriptor-proto-type-type-sfixed32+
      +field-descriptor-proto-type-type-float+)
     4)

    ((+field-descriptor-proto-type-type-fixed64+
      +field-descriptor-proto-type-type-sfixed64+
      +field-descriptor-proto-type-type-double+)
     8)

    ((+field-descriptor-proto-type-type-int32+
      +field-descriptor-proto-type-type-enum+)
     ;; XXXXXXXXXXXXXXXXXXXX see coded_stream.h for how this can be coded more efficiently:
     ;; VarintSize32SignExtended().  The result is 10 if value is negative, else it's
     ;; length32(value).
     `(varint:length64 (cl:ldb (cl:byte 64 0) ,form)))

    (+field-descriptor-proto-type-type-int64+ `(varint:length64 (cl:ldb (cl:byte 64 0) ,form)))
    (+field-descriptor-proto-type-type-uint32+ `(varint:length32 ,form))
    (+field-descriptor-proto-type-type-uint64+ `(varint:length64 ,form))

    (+field-descriptor-proto-type-type-sint32+
     `(varint:length32 (wire-format:zig-zag-encode32 ,form)))
    (+field-descriptor-proto-type-type-sint64+
     `(varint:length64 (wire-format:zig-zag-encode64 ,form)))

    (+field-descriptor-proto-type-type-string+
     `(let ((s (pb::%utf8-string-length% ,form))) (+ s (varint:length32 s))))
    (+field-descriptor-proto-type-type-bytes+
     `(let ((s (cl:length ,form))) (+ s (varint:length32 s))))

    (+field-descriptor-proto-type-type-group+ `(pb:octet-size ,form))
    (+field-descriptor-proto-type-type-message+
     `(let ((s (pb:octet-size ,form))) (+ s (varint:length32 s))))))

(defun default-value (field-descriptor-proto)
  (if (has-default-value field-descriptor-proto)
      (let ((default (default-value field-descriptor-proto)))
        (ecase (type field-descriptor-proto)
          (+field-descriptor-proto-type-type-bool+ (if (string= default "true") t nil))
          ((+field-descriptor-proto-type-type-int32+
            +field-descriptor-proto-type-type-sfixed32+
            +field-descriptor-proto-type-type-sint32+
            +field-descriptor-proto-type-type-enum+ ; default enum value
            +field-descriptor-proto-type-type-uint32+
            +field-descriptor-proto-type-type-fixed32+
            +field-descriptor-proto-type-type-int64+
            +field-descriptor-proto-type-type-sfixed64+
            +field-descriptor-proto-type-type-sint64+
            +field-descriptor-proto-type-type-uint64+
            +field-descriptor-proto-type-type-fixed64+)
           (parse-integer default))
          (+field-descriptor-proto-type-type-double+ (coerce (read default) 'double-float))
          (+field-descriptor-proto-type-type-float+ (coerce (read default) 'single-float))
          (+field-descriptor-proto-type-type-string+ `(pb:string-field ,default))
          (+field-descriptor-proto-type-type-bytes+
           `(make-array ,(length default)
                        :element-type '(unsigned-byte 8)
                        :initial-contents ',(loop for c across default collect (char-code c))))
          ((+field-descriptor-proto-type-type-group+
            +field-descriptor-proto-type-type-message+)
           nil)))
      (ecase (type field-descriptor-proto)
        (+field-descriptor-proto-type-type-bool+ nil)
        ((+field-descriptor-proto-type-type-int32+
          +field-descriptor-proto-type-type-sfixed32+
          +field-descriptor-proto-type-type-sint32+
          +field-descriptor-proto-type-type-enum+ ; default enum value
          +field-descriptor-proto-type-type-uint32+
          +field-descriptor-proto-type-type-fixed32+
          +field-descriptor-proto-type-type-int64+
          +field-descriptor-proto-type-type-sfixed64+
          +field-descriptor-proto-type-type-sint64+
          +field-descriptor-proto-type-type-uint64+
          +field-descriptor-proto-type-type-fixed64+)
         0)
        (+field-descriptor-proto-type-type-double+ 0d0)
        (+field-descriptor-proto-type-type-float+ 0f0)
        (+field-descriptor-proto-type-type-string+ `(pb:string-field ""))
        (+field-descriptor-proto-type-type-bytes+ `(make-array 0 :element-type '(unsigned-byte 8)))
        ((+field-descriptor-proto-type-type-group+
          +field-descriptor-proto-type-type-message+)
         nil))))


(defun print-code (form)
  (with-standard-io-syntax
    (let ((*package* (find-package 'protocol-buffer))
          (*print-case* :downcase)
          (*print-pretty* t))
      (print form))))



(defun main ()
  (values))

;;;; Copyright 2008 Google Inc.  All rights reserved.

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

;;;; Author: Robert Brown <robert.brown@gmail.com>

;;;; Protocol buffer support code in the PROTOCOL-BUFFER package.

;;;; All symbols outside the PROTOCOL-BUFFER package are explicitly qualified in this file because
;;;; the PROTOCOL-BUFFER package does not use the COMMON-LISP package, or any others.

(in-package #:protocol-buffer)

(cl:defclass protocol-buffer ()
  ()
  (:documentation "Superclass of all protocol buffer classes."))

;;;; Functions supported by all protocol buffers.

(cl:defgeneric clear (protocol-buffer)
  (:documentation "Sets the slots of PROTOCOL-BUFFER to their default values."))

(cl:defgeneric is-initialized (protocol-buffer)
  (:documentation "Are all the slots of PROTOCOL-BUFFER initialized?"))

(cl:defgeneric octet-size (protocol-buffer)
  (:documentation
   "Returns the number of octets required to represent PROTOCOL-BUFFER when it
is encoded."))

(cl:defgeneric serialize (protocol-buffer buffer index limit)
  (:documentation
   "Serializes PROTOCOL-BUFFER into BUFFER.  Starts writing at position INDEX of
BUFFER, but does not write into position LIMIT or higher.  If serialization
demands writing past LIMIT, then signals PROTOCOL-BUFFER-WRITE-ERROR.

OCTET-SIZE must be called immediately before SERIALIZE because PROTOCOL-BUFFER
instances cache size information."))

(cl:defgeneric merge-from-array (protocol-buffer buffer start limit)
  (:documentation
   "Merges the contents of the encoded protocol buffer stored in BUFFER into
PROTOCOL-BUFFER.  When reading from BUFFER, begins at position START and does
not read from position LIMIT or higher.  If deserialization demands reading
beyond LIMIT, then signals PROTOCOL-BUFFER-READ-ERROR."))

(cl:defgeneric merge-from-message (protocol-buffer source-protocol-buffer)
  (:documentation "Merges the contents of SOURCE-PROTOCOL-BUFFER into PROTOCOL-BUFFER."))

;;;; Common Lisp does not mandate IEEE floating point, so some Lisp systems have support for IEEE
;;;; infinities and NaNs, while others do not.

;;; Infinities

(cl:define-symbol-macro +single-float-positive-infinity+
  #+abcl extensions:single-float-positive-infinity
  #+ecl ext:single-float-positive-infinity
  #+ccl 1f++0
  #+lispworks 1f++0
  #+sbcl sb-ext:single-float-positive-infinity
  #-(or abcl ccl ecl lispworks sbcl) (cl:error "+single-float-positive-infinity+ unimplemented"))

(cl:define-symbol-macro +single-float-negative-infinity+
  #+abcl extensions:single-float-negative-infinity
  #+ecl ext:single-float-negative-infinity
  #+ccl -1f++0
  #+lispworks -1f++0
  #+sbcl sb-ext:single-float-negative-infinity
  #-(or abcl ccl ecl lispworks sbcl) (cl:error "+single-float-negative-infinity+ unimplemented"))

(cl:define-symbol-macro +double-float-positive-infinity+
  #+abcl extensions:double-float-positive-infinity
  #+ecl ext:double-float-positive-infinity
  #+ccl ccl::double-float-positive-infinity
  #+lispworks 1d++0
  #+sbcl sb-ext:double-float-positive-infinity
  #-(or abcl ccl ecl lispworks sbcl) (cl:error +double-float-positive-infinity+ "unimplemented"))

(cl:define-symbol-macro +double-float-negative-infinity+
  #+abcl extensions:double-float-negative-infinity
  #+ecl ext:double-float-negative-infinity
  #+ccl ccl::double-float-negative-infinity
  #+lispworks -1d++0
  #+sbcl sb-ext:double-float-negative-infinity
  #-(or abcl ccl ecl lispworks sbcl) (cl:error "+double-float-negative-infinity+ unimplemented"))

;;; NaNs

(cl:define-symbol-macro +single-float-nan+
  #+abcl (system:make-single-float -1)
  #+ccl 1f+-0
  #+lispworks system::*single-float-nan*
  #+sbcl #.(sb-kernel:make-single-float -1)
  #-(or abcl ccl lispworks sbcl) (cl:error "+single-float-nan+ unimplemented"))

(cl:define-symbol-macro +double-float-nan+
  #+abcl (system:make-double-float (cl:ash -1 32))
  #+ccl ccl::double-float-nan
  #+lispworks system::*double-float-nan*
  #+sbcl #.(sb-kernel:make-double-float -1 0)
  #-(or abcl ccl lispworks sbcl) (cl:error "+double-float-nan+ unimplemented"))

;;;; Protocol buffer string fields

(cl:defclass %sf% ()
  ((%octets% :accessor %octets%
             :initarg :octets
             :initform (cl:make-array 0 :element-type '(cl:unsigned-byte 8))
             :type com.google.base:octet-vector
             :documentation "Octet vector that holds the string field's value."))
  (:documentation "A protocol buffer string field."))

(cl:defmethod cl:print-object ((string-field %sf%) stream)
  (cl:print-unreadable-object (string-field stream :type cl:t :identity cl:nil)
    (cl:format stream "~S" (string-value string-field))))

(cl:declaim (cl:ftype (cl:function ((cl:or cl:string com.google.base:octet-vector %sf%))
                                   (cl:values %sf% cl:&optional))
                      string-field))

(cl:defun string-field (value)
  "Returns a new %SF% instance initialized to hold VALUE, which much be either a Lisp
string or a vector of UTF-8 encoded octets."
  (cl:let ((octets
             (cl:etypecase value
               (cl:string (com.google.base:string-to-utf8-octets value))
               (com.google.base:octet-vector value)
               (%sf% (utf8-string-value value)))))
    (cl:make-instance '%sf% :octets octets)))

(cl:declaim (cl:ftype (cl:function (%sf%) (cl:values cl:string cl:&optional)) string-value))

(cl:defun string-value (string-field)
  "Returns STRING-FIELD's value as a Lisp string."
  (cl:declare (cl:type %sf% string-field))
  (com.google.base:utf8-octets-to-string (%octets% string-field)))

(cl:declaim (cl:ftype (cl:function (%sf%) (cl:values com.google.base:octet-vector cl:&optional))
                      utf8-string-value))

(cl:defun utf8-string-value (string-field)
  "Returns STRING-FIELD's value as a UTF-8 encoded vector of octets."
  (cl:declare (cl:type %sf% string-field))
  (cl:copy-seq (cl:slot-value string-field '%octets%)))

(cl:declaim (cl:ftype (cl:function (%sf%) (cl:values com.google.base:vector-index cl:&optional))
                      %utf8-string-length%))

(cl:defun %utf8-string-length% (string-field)
  "Returns the length in octets of STRING-FIELD's value."
  (cl:declare (cl:type %sf% string-field))
  (cl:length (cl:slot-value string-field '%octets%)))

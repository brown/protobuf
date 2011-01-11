
;;;;    protocol-buffer.lisp


;; Copyright 2008, Google Inc.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;     * Neither the name of Google Inc. nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package #:protocol-buffer)


;; All COMMON-LISP symbols are explicitly qualified in this file because
;; the PROTOCOL-BUFFER package does not use the COMMON-LISP package.


(cl:defclass protocol-buffer ()
  ()
  (:documentation "Superclass of all protocol buffer classes."))


;;; Functions supported by all protocol buffers.


(cl:defgeneric clear (protocol-buffer)
  (:documentation "Set the slots of PROTOCOL-BUFFER to default values."))


(cl:defgeneric is-initialized (protocol-buffer)
  (:documentation "Are all the slots of PROTOCOL-BUFFER initialized?"))

(cl:defgeneric octet-size (protocol-buffer)
  (:documentation "Return the number of octets required to represent
PROTOCOL-BUFFER when it is encoded."))

(cl:defgeneric serialize (protocol-buffer buffer index limit)
  (:documentation "Serialize PROTOCOL-BUFFER into BUFFER.  Start encoding at
position INDEX of BUFFER and do not write into position LIMIT or higher.  If
serialization demands writing past LIMIT, then signal
PROTOCOL-BUFFER-WRITE-ERROR.  NOTE: OCTET-SIZE must be called immediately
before SERIALIZE because the protocol buffer caches size information."))

(cl:defgeneric merge-from-array (protocol-buffer buffer start limit)
  (:documentation "Merge the contents of the encoded protocol buffer stored in
BUFFER into PROTOCOL-BUFFER.  When reading from BUFFER, begin at position
START and do not read at position LIMIT or higher.  If deserialization demands
reading beyond LIMIT, then signal PROTOCOL-BUFFER-READ-ERROR."))

(cl:defgeneric merge-from-message (protocol-buffer source-protocol-buffer)
  (:documentation "Merge the contents SOURCE-PROTOCOL-BUFFER into
PROTOCOL-BUFFER."))


;; Common Lisp does not define floating point infinities and NaNs, so some
;; platforms support them and others do not.

;; Infinities

(cl:define-symbol-macro +single-float-positive-infinity+
  #+abcl extensions:single-float-positive-infinity
  #+sbcl sb-ext:single-float-positive-infinity
  #-(or abcl sbcl) (cl:error "unimplemented"))

(cl:define-symbol-macro +single-float-negative-infinity+
  #+abcl extensions:single-float-negative-infinity
  #+sbcl sb-ext:single-float-negative-infinity
  #-(or abcl sbcl) (cl:error "unimplemented"))

(cl:define-symbol-macro +double-float-positive-infinity+
  #+abcl extensions:double-float-positive-infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #-(or abcl sbcl) (cl:error "unimplemented"))

(cl:define-symbol-macro +double-float-negative-infinity+
  #+abcl extensions:double-float-negative-infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #-(or abcl sbcl) (cl:error "unimplemented"))

;; NaNs

(cl:define-symbol-macro +single-float-nan+
  #+sbcl #.(sb-kernel:make-single-float -1)
  #-sbcl (cl:error "unimplemented"))

(cl:define-symbol-macro +double-float-nan+
  #+sbcl #.(sb-kernel:make-double-float -1 0)
  #-sbcl (cl:error "unimplemented"))



;; Can anything useful be done with declarations such as these?

;; (cl:declaim (cl:ftype (cl:function (protocol-buffer) (cl:values)) clear))
;; (cl:declaim (cl:ftype (cl:function (protocol-buffer) cl:boolean) is-initialized))
;; (cl:declaim (cl:ftype (cl:function (protocol-buffer) cl:fixnum) octet-size))
;; (cl:declaim (cl:ftype (cl:function (protocol-buffer
;;                                     base:octet-vector
;;                                     base:octet-vector-index
;;                                     base:octet-vector-index)
;;                                    (cl:values))
;;                       serialize))
;; (cl:declaim (cl:ftype (cl:function (protocol-buffer
;;                                     base:octet-vector
;;                                     base:octet-vector-index
;;                                     base:octet-vector-index)
;;                                    (cl:values))
;;                       merge))

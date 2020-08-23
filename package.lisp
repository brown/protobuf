;;;; Copyright 2010, Google Inc. All rights reserved.

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

(in-package #:common-lisp-user)

(defpackage #:protocol-buffer
  (:documentation "Machine generated protocol buffers.")
  (:nicknames #:pb)
  ;; Packages containing machine-generated protocol buffer code, including this one, use no other
  ;; packages, not even COMMON-LISP, so protocol buffer code must explicitly qualify references to
  ;; symbols outside the PROTOCOL-BUFFER package.  The benefit of this approach is that protocol
  ;; buffers can use field names such as "second" or "debug", which when translated into Lisp would
  ;; ordinarily conflict with symbols in the COMMON-LISP package.
  (:use)
  ;; Machine-generated protocol buffer code exports additional symbols for each enum tag, protocol
  ;; buffer constructor, field accessor, etc.
  (:export #:protocol-buffer
           ;; Operations on protocol buffers
           #:clear
           #:is-initialized
           #:octet-size
           #:merge-from-array
           #:merge-from-message
           #:serialize
           ;; String fields
           #:string-field
           #:string-value
           #:utf8-string-value))

(defpackage #:wire-format
  (:documentation "Wire format for protocol buffers.")
  (:use #:common-lisp #:com.google.base)
  (:export ;; Constants
           #:+end-group+
           #:+fixed32+
           #:+fixed64+
           #:+length-delimited+
           #:+start-group+
           #:+varint+
           ;; Types
           #:field-number
           #:wire-type
           ;; Conditions
           #:protocol-error
           #:encoding-error
           #:buffer-overflow
           #:parsing-error
           #:data-exhausted
           #:value-out-of-range
           #:alignment
           ;; Functions
           #:parse-tag
           #:skip-field
           #:write-boolean-carefully
           #:read-boolean-carefully
           #:write-int32-carefully
           #:write-uint32-carefully
           #:write-int64-carefully
           #:write-uint64-carefully
           #:read-uint32-carefully
           #:read-int32-carefully
           #:read-uint64-carefully
           #:read-int64-carefully
           #:write-single-float-carefully
           #:write-double-float-carefully
           #:read-single-float-carefully
           #:read-double-float-carefully
           #:write-octets-carefully
           #:read-octets-carefully
           #:zig-zag-encode32
           #:zig-zag-decode32
           #:zig-zag-encode64
           #:zig-zag-decode64))

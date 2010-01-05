
;;;;    package.lisp


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


(in-package #:common-lisp-user)


(defpackage #:optimize
  (:documentation "Compiler optimization settings")
  (:use #:common-lisp)
  (:export #:+default+
           #:+fast-unsafe+))

(defpackage #:base
  (:documentation "Basic types and other low-level stuff")
  (:use #:common-lisp)
  (:export #:defconst
           #:int32
           #:int64
           #:uint32
           #:uint64
           #:int32-to-uint32
           #:int32-to-uint64
           #:int64-to-uint64
           #:octet
           #:octet-vector
           #:octet-vector-index
           #:+octet-vector-index-bits+
           #:+illegal-octet-vector-index+
           #:make-octet-vector
           #:string-to-utf8-octets
           #:utf8-octets-to-string))

(defpackage #:varint
  (:documentation "Variable-size encoding and decoding of integers and floats")
  (:use #:common-lisp
        #:base)
  (:export ;; Constants
           #:+max-bytes-32+
           #:+max-bytes-64+
           ;; Conditions
           #:varint-error
           #:encoding-error
           #:buffer-overflow
           #:parsing-error
           #:data-exhausted
           #:value-out-of-range
           #:alignment
           ;; Functions
           #:encode-uint32
           #:encode-uint32-carefully
           #:encode-uint64
           #:encode-uint64-carefully
           #:parse-uint32
           #:parse-uint32-carefully
           #:parse-uint31-carefully
           #:parse-uint64
           #:parse-uint64-carefully
           #:parse-int64-carefully
           #:parse-int32-carefully
           #:skip32
           #:skip64
           #:skip32-backward-slow
           #:skip64-backward-slow
           #:skip32-backward
           #:skip64-backward
           #:parse32-backward-slow
           #:parse64-backward-slow
           #:parse32-backward
           #:parse64-backward
           #:length32
           #:length64
           #:zig-zag-encode32
           #:zig-zag-decode32
           #:zig-zag-encode64
           #:zig-zag-decode64
           ))

(defpackage #:protocol
  (:documentation "Protocol buffer support functions")
  (:use #:common-lisp
        #:base)
  (:export ;; Conditions
           #:protocol-error
           #:encoding-error
           #:buffer-overflow
           #:parsing-error
           #:data-exhausted
           #:value-out-of-range
           #:alignment
           ;; Types
           #:protocol-tag
           ;; Functions
           #:skip-element
           #:write-boolean-carefully
           #:read-boolean-carefully
           #:write-int32-carefully
           #:write-uint32-carefully
           #:write-uint32
           #:write-int64-carefully
           #:write-uint64-carefully
           #:read-uint32-carefully
           #:read-int32-carefully
           #:read-uint32
           #:read-uint64-carefully
           #:read-int64-carefully
           #:write-single-float-carefully
           #:write-double-float-carefully
           #:read-single-float-carefully
           #:read-double-float-carefully
           #:write-octets-carefully
           #:read-octets-carefully))

(defpackage #:protocol-buffer
  (:documentation "Machine generated protocol buffers")
  (:nicknames #:pb)
  ;; We use no packages, not even COMMON-LISP, so machine-generated protocol
  ;; buffer code must explicitly qualify references to symbols outside the
  ;; PROTOCOL-BUFFER package.  The benefit of this approach is that protocol
  ;; buffers can use field names such as SECOND or DEBUG, which live in the
  ;; COMMON-LISP package, without causing symbol conflicts.
  (:use)
  ;; String fields in protocol buffers are really octet vectors, but they
  ;; are often used to store UTF-8 encoded strings.
  (:import-from #:base
                #:octet-vector
                #:octet-vector-index
                #:make-octet-vector
                #:string-to-utf8-octets
                #:utf8-octets-to-string)
  ;; Machine generated protocol buffer code exports additional symbols for
  ;; each enum tag, protocol buffer constructor, field accessor, etc.
  (:export #:protocol-buffer
           #:clear
           #:is-initialized
           #:octet-size
           #:merge
           #:serialize))

(defpackage #:portable-float
  (:documentation "Access the bits of IEEE floating point numbers")
  (:use #:common-lisp)
  (:export #:mask-and-sign-extend
           #:single-float-bits
           #:double-float-bits
           #:make-single-float
           #:make-double-float))

(defpackage #:proto-lisp-test
  (:documentation "Test the Lisp implementation of protocol buffers")
  (:use #:common-lisp)
  (:import-from #:base
                #:defconst)
  (:export #:test))

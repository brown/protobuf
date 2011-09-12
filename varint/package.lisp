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

(in-package #:common-lisp-user)

(defpackage #:varint
  (:documentation "Variable-size encoding and decoding of integers and floats")
  (:use #:common-lisp #:com.google.base)
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
           #:length64))

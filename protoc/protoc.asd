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

(defsystem protoc
  :name "Lisp Protoc"
  :description "Protocol buffer compiler Common Lisp plugin"
  :version "0.1.0"
  :author "Robert Brown"
  :license "See file COPYING and the copyright messages in individual files."
  :defsystem-depends-on (protobuf)
  :depends-on (cl-ppcre
               com.google.base)
  :in-order-to ((test-op (test-op protoc-test)))
  :components
  ((:protobuf-source-file "descriptor" :proto-pathname "../google/protobuf/descriptor")
   (:protobuf-source-file "plugin"
    :proto-pathname "../google/protobuf/compiler/plugin"
    :depends-on ("descriptor")
    :proto-search-path ("../"))
   (:file "package" :depends-on ("descriptor" "plugin"))
   (:file "protoc" :depends-on ("package" "descriptor" "plugin"))))

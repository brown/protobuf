
;;;;    protobuf-test.asd


;; Copyright 2010, Google Inc. All rights reserved.

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


(cl:in-package #:common-lisp-user)

(defpackage #:protobuf-test-system
  (:documentation "System definitions for protocol buffer test code.")
  (:use #:common-lisp #:asdf))

(in-package #:protobuf-test-system)


(defsystem protobuf-test
  :name "Protocol Buffer Test"
  :description "Protocol buffer test code"
  :long-description "Code to test the protocol buffer compiler and support libraries."
  :version "0.4"
  :author "Robert Brown"
  :licence "See file COPYING and the copyright messages in individual files."
  :defsystem-depends-on (:protobuf)
  :components
  ((:static-file "golden")
   (:cl-source-file "varint-test")
   (:cl-source-file "message-test" :depends-on ("unittest"))
   (:cl-source-file "wire-format-test")

   ;; Old protocol buffer tests
   ;; XXXX: Delete these when the new proto2 tests cover all the functionality.

   (:cl-source-file "proto-lisp-test" :depends-on ("testproto1" "testproto2"))
   ;; Two protocol buffers used by the old tests.
   (:protobuf-source-file "testproto1")
   (:protobuf-source-file "testproto2")

   ;; Test protocol buffers and protobuf definitions used by the proto2
   ;; compiler.

   (:protobuf-source-file "descriptor"
    :proto-pathname "google/protobuf/descriptor")
   (:protobuf-source-file "unittest_import"
    :proto-pathname "google/protobuf/unittest_import")
   (:protobuf-source-file "unittest"
    :proto-pathname "google/protobuf/unittest"
    :depends-on ("unittest_import")
    :proto-search-path ("./"))))


(defmethod operation-done-p ((operation test-op)
                             (component (eql (find-system :protobuf-test))))
  nil)

(defmethod perform ((operation test-op)
                    (component (eql (find-system :protobuf-test))))
  (funcall (intern (string '#:test) '#:varint))
  (funcall (intern (string '#:test) '#:proto-lisp-test))
  (funcall (intern (string '#:test) '#:wire-format-test))
  (funcall (intern (string '#:test) '#:message-test)))

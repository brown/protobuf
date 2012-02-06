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

;;;; Test protoc code

(in-package #:common-lisp-user)

(defpackage #:protoc-test
  (:documentation "Test the PROTOC package.")
  (:use #:common-lisp
        #:com.google.base
        #:hu.dwim.stefil
        #:protoc)
  (:export #:test-protoc))

(in-package #:protoc-test)
(declaim #.*optimize-default*)

(defsuite (test-protoc :in root-suite) ()
  (run-child-tests))

(in-suite test-protoc)

(deftest hyphenation ()
  (flet ((verify (name expected)
           (is (string= (string-downcase (protoc::hyphenate-studly-caps name))
                        expected))))
    (verify "HELLO" "hello")
    (verify "HelloWorld" "hello-world")
    (verify "helloWorld" "hello-world")
    (verify "HELLOWorld" "hello-world")
    (verify "Hello-World" "hello-world")
    (verify "spaceX" "space-x")
    (verify "SpaceX" "space-x")
    (verify "RFC1234" "rfc-1234")
    (verify "Rfc1234" "rfc-1234")
    (verify "Html5Manual" "html-5-manual")
    (verify "X15Airplane" "x-15-airplane")))

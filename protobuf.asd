
;;;;    protobuf.asd


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


(cl:in-package #:common-lisp)

(defpackage #:protobuf-system
  (:documentation "System definitions for protocol buffer code.")
  (:use #:common-lisp
        #:asdf))

(in-package #:protobuf-system)

(defsystem protobuf
    :name "Protocol Buffer"
    :description "Protocol Buffer code"
    :long-description "A Common Lisp implementation of Google's protocol
 buffer compiler and support libraries."
    :version "0.2"
    :author "Robert Brown"
    :licence "See file COPYING and the copyright messages on individual files."

    :perform (load-op :after (operation protobuf)
               (pushnew :protobuf cl:*features*)
               (provide 'protobuf))

    :depends-on (#-(or allegro clisp sbcl) :trivial-utf-8)

    :components
    ((:cl-source-file "package")

     #-(or abcl allegro cmu sbcl)
     (:module "sysdep"
      :pathname ""              ; this module's files are not in a subdirectory
      :depends-on ("package")
      :components ((:cl-source-file "portable-float")))

     (:cl-source-file "optimize" :depends-on ("package"))
     (:cl-source-file "base" :depends-on ("optimize"))
     (:cl-source-file "varint"  :depends-on ("base"))
     (:cl-source-file "varint-test" :depends-on ("varint"))
     (:cl-source-file "proto"
      :depends-on ("base" #-(or abcl allegro cmu sbcl) "sysdep"))
     (:cl-source-file "protocol-buffer" :depends-on ("base"))
     (:cl-source-file "proto-lisp-test"
      :depends-on ("base" "proto-test" "testprotocol"))

     ;; Machine generated protocol buffer code.

     (:cl-source-file "testprotocol"
      :depends-on ("package" "base" "varint" "protocol-buffer"))
     (:cl-source-file "proto-test"
      :depends-on ("package" "base" "varint" "protocol-buffer"))

     (:static-file "COPYING")
     (:static-file "README")
     (:static-file "TODO")
     (:static-file "golden")
     ))


;;;;    protobuf.asd


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

(defpackage #:protobuf-system
  (:documentation "System definitions for protocol buffer code.")
  (:use #:common-lisp #:asdf))

(in-package #:protobuf-system)


;;; Teach ASDF how to convert protocol buffer definition files into Lisp.

;;; We define two kinds of components, PROTO-FILE for files containing
;;; protocol buffer definitions, and CL-PB-IMPL for the corresponding
;;; machine-generated Lisp code.


(defparameter *protoc* #p"google-protobuf/src/protoc")


(defclass proto-file (source-file)
  ()
  (:documentation "Protocol buffer definition file"))

(defmethod source-file-type ((component proto-file) (module module)) "proto")

(defmethod input-files ((operation compile-op) (component proto-file))
  (append (list *protoc*) (call-next-method)))

(defmethod output-files ((operation compile-op) (component proto-file))
  (list (merge-pathnames
         (make-pathname :name (pathname-name (component-pathname component))
                        :type "lisp")
         (asdf::component-parent-pathname component))))

(defmethod perform ((operation compile-op) (component proto-file))
  (let* ((source-file (component-pathname component))
         (output-file (first (output-files operation component)))
         ;; XXXX
         (compiler-source #p"google-protobuf/src/"))
    (zerop (run-shell-command "~A --proto_path=~A:~A --lisp_out=~A ~A"
                              (namestring *protoc*)
                              (directory-namestring source-file)
                              (directory-namestring compiler-source)
                              (directory-namestring output-file)
                              (namestring source-file)))))

(defmethod perform ((operation load-op) (component proto-file))
  nil)

(defclass cl-pb-impl (cl-source-file)
  ()
  (:documentation "Machine-generated Common Lisp implementation of protocol
buffer messages."))

;; Machine-generated protocol buffer Lisp files cannot be compiled or loaded
;; unless various dependencies are already loaded.  The package.lisp file
;; defines the package containing protobuf code.  The other Lisp files
;; define classes or in-line functions referenced by the protobuf code.

(defmethod component-depends-on ((op compile-op) (component cl-pb-impl))
  (append
   '((compile-op "package" "base" "protocol-buffer" "varint" "wire-format"))
   (call-next-method)))

(defmethod component-depends-on ((op load-op) (component cl-pb-impl))
  (append
   '((load-op "package" "base" "protocol-buffer" "varint" "wire-format"))
   (call-next-method)))


;;; A Common Lisp implementation of Google's protocol buffers


(defsystem protobuf
  :name "Protocol Buffer"
  :description "Protocol buffer code"
  :long-description "A Common Lisp implementation of Google's protocol
buffer compiler and support libraries."
  :version "0.3.2"
  :author "Robert Brown"
  :licence "See file COPYING and the copyright messages in individual files."

  ;; After loading the system, announce its availability.
  :perform (load-op :after (operation component)
             (pushnew :protobuf cl:*features*)
             (provide 'protobuf))

  :depends-on (#-(or allegro clisp sbcl) :trivial-utf-8)

  :components
  ((:static-file "COPYING")
   (:static-file "README")
   (:static-file "TODO")
   (:static-file "golden")

   (:cl-source-file "package")

   #-(or abcl allegro cmu sbcl)
   (:module "sysdep"
    :pathname ""           ; this module's files are not in a subdirectory
    :depends-on ("package")
    :components ((:cl-source-file "portable-float")))

   (:cl-source-file "optimize" :depends-on ("package"))
   (:cl-source-file "base" :depends-on ("package" "optimize"))
   (:cl-source-file "varint"  :depends-on ("package" "optimize" "base"))
   (:cl-source-file "varint-test" :depends-on ("package"))
   (:cl-source-file "protocol-buffer" :depends-on ("package"))
   (:cl-source-file "message-test"
    :depends-on ("optimize" "base" "protocol-buffer" "unittest"))
   ;; The varint dependency is needed because some varint functions are
   ;; declared in line and so must be loaded before wire-format is compiled.
   (:cl-source-file "wire-format"
    :depends-on ("package" "base" "optimize" "varint"
                 #-(or abcl allegro cmu sbcl) "sysdep"))
   (:cl-source-file "wire-format-test" :depends-on ("package" "optimize"))

   ;; Old protocol buffer tests
   ;; XXXX: Delete these when the new proto2 tests cover all the functionality.

   (:cl-source-file "proto-lisp-test"
    :depends-on ("base" "testproto1" "testproto2"))
   ;; Two protocol buffers used by the old tests.
   (:proto-file "testproto1-pb" :pathname "testproto1")
   (:proto-file "testproto2-pb" :pathname "testproto2")
   (:cl-pb-impl "testproto1" :depends-on ("testproto1-pb"))
   (:cl-pb-impl "testproto2" :depends-on ("testproto2-pb"))

   ;; Test protocol buffers and protobuf definitions used by the proto2
   ;; compiler.

   (:proto-file "descriptor-pb"
    :pathname "google-protobuf/src/google/protobuf/descriptor")
   (:proto-file "unittest_import-pb"
    :pathname "google-protobuf/src/google/protobuf/unittest_import")
   (:proto-file "unittest-pb"
    :pathname "google-protobuf/src/google/protobuf/unittest")
   (:cl-pb-impl "descriptor" :depends-on ("descriptor-pb"))
   (:cl-pb-impl "unittest_import" :depends-on ("unittest_import-pb"))
   (:cl-pb-impl "unittest" :depends-on ("unittest-pb" "unittest_import"))
   ))

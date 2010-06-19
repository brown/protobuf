
;;;;    protobuf-example.asd


(cl:in-package #:common-lisp-user)

(defpackage #:protobuf-example-system
  (:documentation "System definitions for protocol buffer example code.")
  (:use #:common-lisp #:asdf))

(in-package #:protobuf-example-system)


(defsystem protobuf-example
  :name "Protocol Buffer Example"
  :description "Protocol buffer example code"
  :long-description "Example code that shows how to compile and use protocol buffer definitions."
  :version "0.3.4"
  :author "Robert Brown"
  :licence "See file COPYING and the copyright messages in individual files."
  :depends-on (:protobuf)
  :components ((:static-file "Makefile")
               (:static-file "README")
               (:cl-source-file "package")
               (protobuf-system:proto-file "addressbook")
               (:cl-source-file "address-book"
                :depends-on ("package" "addressbook"))))

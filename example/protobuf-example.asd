
;;;;    protobuf-example.asd

(defsystem protobuf-example
  :name "Protocol Buffer Example"
  :description "Protocol buffer example code"
  :long-description "Example code that shows how to compile and use protocol buffer definitions."
  :version "0.8.0"
  :author "Robert Brown"
  :license "See file COPYING and the copyright messages in individual files."
  :defsystem-depends-on (protobuf)
  :components
  ((:static-file "Makefile")
   (:static-file "README")
   ;; The file addressbook.proto contains protocol buffer definitions.  Using the ASDF
   ;; component type PROTOBUF-SOURCE-FILE causes the proto file to be converted into Lisp
   ;; code, which is then compiled and/or loaded.
   (:protobuf-source-file "addressbook")
   (:file "package" :depends-on ("addressbook"))
   (:file "address-book" :depends-on ("package" "addressbook"))))

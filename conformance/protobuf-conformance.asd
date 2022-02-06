(defsystem protobuf-conformance
  :name "Protocol buffer conformance test"
  :description "Protocol buffer conformance test code"
  :version "0.2.0"
  :author "Robert Brown <robert.brown@gmail.com>"
  :license "See file COPYING and the copyright messages in individual files."
  :defsystem-depends-on (protobuf)
  :depends-on (com.google.base nibbles)
  :components
  ((:protobuf-source-file "conformance")

   (:protobuf-source-file "test_messages_proto2"
    :proto-pathname "google/protobuf/test_messages_proto2")

   (:protobuf-source-file "any" :proto-pathname "google/protobuf/any")
   (:protobuf-source-file "duration" :proto-pathname "google/protobuf/duration")
   (:protobuf-source-file "field_mask" :proto-pathname "google/protobuf/field_mask")
   (:protobuf-source-file "struct" :proto-pathname "google/protobuf/struct")
   (:protobuf-source-file "timestamp" :proto-pathname "google/protobuf/timestamp")
   (:protobuf-source-file "wrappers" :proto-pathname "google/protobuf/wrappers")
   (:protobuf-source-file "test_messages_proto3"
    :proto-pathname "google/protobuf/test_messages_proto3"
    :depends-on ("any" "duration" "field_mask" "struct" "timestamp" "wrappers"))

   (:file "conformance-lisp"
    :depends-on ("conformance" "test_messages_proto2" "test_messages_proto3"))
   ))

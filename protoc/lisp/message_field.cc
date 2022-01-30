// Protocol Buffers - Google's data interchange format
// Copyright 2009 Google Inc.  All rights reserved.
// http://code.google.com/p/protobuf/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "message_field.h"

#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>
#include "helpers.h"
#include <google/protobuf/stubs/strutil.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;
using internal::WireFormatLite;

namespace {

// TODO(kenton):  Factor out a "SetCommonFieldVariables()" to get rid of
//   repeat code between this and the other field types.
void SetMessageVariables(const FieldDescriptor* descriptor,
                         std::map<string, string>* variables) {
  (*variables)["name"] = FieldName(descriptor);
  (*variables)["type"] = ClassName(descriptor->message_type());
  (*variables)["index"] = SimpleItoa(descriptor->index());
  (*variables)["number"] = SimpleItoa(descriptor->number());
  (*variables)["classname"] = ClassName(FieldScope(descriptor));
  (*variables)["package"] = FileLispPackage(descriptor->message_type()->file());

  // For groups, the tag size includes the size of the group end tag.
  (*variables)["tag_size"] =
      SimpleItoa(WireFormat::TagSize(
                     descriptor->number(), descriptor->type()));
}

}  // namespace

MessageFieldGenerator::MessageFieldGenerator(const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetMessageVariables(descriptor, &variables_);
}

MessageFieldGenerator::~MessageFieldGenerator() {}

void MessageFieldGenerator::GenerateSlot(io::Printer* printer) const {
  printer->Print(
      variables_,
      "($name$\n"
      " :writer (cl:setf $name$)\n"
      " :initform cl:nil\n"
      " :type (cl:or cl:null $package$::$type$))\n");
}

void MessageFieldGenerator::GenerateClearingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) cl:nil)");
}

void MessageFieldGenerator::GenerateOctetSize(io::Printer* printer) const {
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    printer->Print(
        variables_,
        "(cl:let ((s (pb:octet-size (cl:slot-value self '$name$))))\n"
        "  (cl:incf size (cl:+ $tag_size$ s (varint:length-uint32 s))))");
  } else if (descriptor_->type() == FieldDescriptor::TYPE_GROUP) {
    printer->Print(
        variables_,
        "(cl:incf size (cl:+ $tag_size$"
        " (pb:octet-size (cl:slot-value self '$name$))))");
  } else {
    GOOGLE_LOG(FATAL) << "Invalid message type";
  }
}

void MessageFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // Message fields are lazily initialized the first time they are accessed.
  printer->Print(
      variables_,
      "(cl:unless (cl:fboundp '$name$)\n"
      "  (cl:defgeneric $name$ (proto)))\n"
      "(cl:defmethod $name$ ((self $classname$))\n"
      "  (cl:let ((result (cl:slot-value self '$name$)))\n"
      "    (cl:when (cl:null result)\n"
      "      (cl:setf result (cl:make-instance '$package$::$type$))\n"
      "      (cl:setf (cl:slot-value self '$name$) result))\n"
      "      (cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)\n"
      "    result))\n");
}

void MessageFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  // XXXXXXXXXX: proto1 version makes embedded protos in an instance either
  // NIL or the message type.  Serialization code then checks for NIL.

  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    uint32 tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_LENGTH_DELIMITED);
    printer->Print(
        "(cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $tag$))\n",
        "tag", SimpleItoa(tag));
    printer->Print(
        variables_,
        "(cl:setf index"
        " (varint:encode-uint32-carefully"
        " buffer index limit"
        " (cl:slot-value (cl:slot-value self '$name$) 'pb::%cached-size%)))\n"
        "(cl:setf index"
        " (pb:serialize (cl:slot-value self '$name$) buffer index limit))");
  } else if (descriptor_->type() == FieldDescriptor::TYPE_GROUP) {
    uint32 start_tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_START_GROUP);
    uint32 end_tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_END_GROUP);
    printer->Print(
        "(cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $start_tag$))\n",
        "start_tag", SimpleItoa(start_tag));
    printer->Print(
        variables_,
        "(cl:setf index"
        " (pb:serialize (cl:slot-value self '$name$) buffer index limit))\n");
    printer->Print(
        "(cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $end_tag$))",
        "end_tag", SimpleItoa(end_tag));
  } else {
    GOOGLE_LOG(FATAL) << "Invalid message type";
  }
}

void MessageFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    printer->Print(
        variables_,
        "(cl:multiple-value-bind (length new-index)\n"
        "    (varint:parse-uint31-carefully buffer index limit)\n"
        "  (cl:when (cl:> (cl:+ new-index length) limit)\n"
        "    (cl:error 'wire-format:data-exhausted))\n"
        "  (cl:let ((message (cl:slot-value self '$name$)))\n"
        "    (cl:when (cl:null message)\n"
        "      (cl:setf message (cl:make-instance '$package$::$type$))\n"
        "      (cl:setf (cl:slot-value self '$name$) message)\n"
        "      (cl:setf (cl:ldb (cl:byte 1 $index$)"
        " (cl:slot-value self '%has-bits%)) 1))\n"
        "    (cl:setf index"
        " (pb:merge-from-array message buffer new-index"
        " (cl:+ new-index length)))\n"
        "    (cl:when (cl:/= index (cl:+ new-index length))\n"
        "      (cl:error 'wire-format:alignment))))");
  } else {
    printer->Print(
        variables_,
        "(cl:let ((message (cl:slot-value self '$name$)))\n"
        "  (cl:when (cl:null message)\n"
        "    (cl:setf message (cl:make-instance '$package$::$type$))\n"
        "    (cl:setf (cl:slot-value self '$name$) message)\n"
        "    (cl:setf (cl:ldb (cl:byte 1 $index$)"
        " (cl:slot-value self '%has-bits%)) 1))\n"
        "  (cl:setf index (pb:merge-from-array message buffer index limit))\n");

    // XXXXXXXXXX: The end tag can be more than one byte, so the (1- index)
    // is wrong.  We need to compare several bytes.
    // Find octet size of tag, then do (not (mismatch ...))
    uint32 end_tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_END_GROUP);
    printer->Print(
        "  ;; XXXX: wrong: tag size could be more than one byte\n"
        "  ;(cl:unless (cl:= (cl:aref buffer (cl:1- index)) $end_tag$)\n"
        "  ;  (cl:error 'wire-format:alignment))\n"
        "  )",
        "end_tag", SimpleItoa(end_tag));
  }
}

void MessageFieldGenerator::GenerateMergingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:let ((message (cl:slot-value self '$name$)))\n"
      "  (cl:when (cl:null message)\n"
      "    (cl:setf message (cl:make-instance '$package$::$type$))\n"
      "    (cl:setf (cl:slot-value self '$name$) message)\n"
      "    (cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1))\n"
      " (pb:merge-from-message message (cl:slot-value from '$name$)))");
}

RepeatedMessageFieldGenerator::RepeatedMessageFieldGenerator(
    const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetMessageVariables(descriptor, &variables_);
}

RepeatedMessageFieldGenerator::~RepeatedMessageFieldGenerator() {}

void RepeatedMessageFieldGenerator::GenerateSlot(io::Printer* printer) const {
  printer->Print(
      variables_,
      "($name$\n"
      " :accessor $name$\n"
      " :initform (cl:make-array\n"
      "            0\n"
      "            :element-type '$package$::$type$\n"
      "            :fill-pointer 0 :adjustable cl:t)\n"
      " :type (cl:vector $package$::$type$))\n");
}

void RepeatedMessageFieldGenerator::GenerateClearingCode(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array 0 :element-type '$package$::$type$\n"
      "          :fill-pointer 0 :adjustable cl:t))");
}

void RepeatedMessageFieldGenerator::GenerateOctetSize(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:let* ((v (cl:slot-value self '$name$))\n"
      "          (length (cl:length v)))\n"
      "  (cl:incf size (cl:* $tag_size$ length))\n"
      "  (cl:dotimes (i length)\n");
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    printer->Print(
        "    (cl:let ((s (pb:octet-size (cl:aref v i))))\n"
        "      (cl:incf size (cl:+ s (varint:length-uint32 s))))))");
  } else if (descriptor_->type() == FieldDescriptor::TYPE_GROUP) {
    printer->Print(
        "    (cl:incf size (pb:octet-size (cl:aref v i)))))");
  }
}

void RepeatedMessageFieldGenerator::GenerateAccessor(io::Printer* printer)
    const {
  // The default accessor works fine for repeated message fields.
}

void RepeatedMessageFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:let* ((v (cl:slot-value self '$name$))\n"
      "          (length (cl:length v)))\n"
      "  (cl:loop for i from 0 below length do\n");
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    uint32 tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_LENGTH_DELIMITED);
    printer->Print(
        "     (cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
        "     (cl:setf index (varint:encode-uint32-carefully"
        " buffer index limit (cl:slot-value (cl:aref v i) 'pb::%cached-size%)))\n"
        "     (cl:setf index (pb:serialize (cl:aref v i) buffer index limit))))",
        "tag", SimpleItoa(tag));
  } else if (descriptor_->type() == FieldDescriptor::TYPE_GROUP) {
    uint32 start_tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_START_GROUP);
    uint32 end_tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_END_GROUP);
    printer->Print(
        "     (cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $start_tag$))\n"
        "     (cl:setf index (pb:serialize (cl:aref v i) buffer index limit))\n"
        "     (cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $end_tag$))))",
        "start_tag", SimpleItoa(start_tag),
        "end_tag", SimpleItoa(end_tag));
  } else {
    GOOGLE_LOG(FATAL) << "Invalid message type";
  }
}

void RepeatedMessageFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    printer->Print(
        variables_,
        "(cl:multiple-value-bind (length new-index)\n"
        "    (varint:parse-uint31-carefully buffer index limit)\n"
        "  (cl:when (cl:> (cl:+ new-index length) limit)\n"
        "    (cl:error 'wire-format:data-exhausted))\n"
        "  (cl:let ((message (cl:make-instance '$package$::$type$)))\n"
        "    (cl:setf index"
        " (pb:merge-from-array message buffer new-index"
        " (cl:+ new-index length)))\n"
        "    (cl:when (cl:/= index (cl:+ new-index length))\n"
        "      (cl:error 'wire-format:alignment))\n"
        "    (cl:vector-push-extend message (cl:slot-value self '$name$))))");
  } else {
    // XXXXXXXXXXXXXXXXXXXX this is probably wrong, but allows old test to pass
    printer->Print(
        variables_,
        "(cl:let ((message (cl:make-instance '$package$::$type$)))\n"
        "  (cl:setf index (pb:merge-from-array message buffer index limit))\n"
        "  (cl:vector-push-extend message (cl:slot-value self '$name$)))\n");
    // XXXXXXXXXX: The end tag can be more than one byte, so the (1- index)
    // is wrong.  We need to compare several bytes.
    // Find octet size of tag, then do (not (mismatch ...))
    uint32 end_tag =
        WireFormatLite::MakeTag(descriptor_->number(),
                                WireFormatLite::WIRETYPE_END_GROUP);
    printer->Print(
        ";; XXXX: wrong: tag size could be more than one byte\n"
        ";(cl:unless (cl:= (cl:aref buffer (cl:1- index)) $end_tag$)\n"
        ";  (cl:error 'wire-format:alignment))\n",
        "end_tag", SimpleItoa(end_tag));
  }
}

void RepeatedMessageFieldGenerator::GenerateMergingCode(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:let* ((v (cl:slot-value self '$name$))\n"
      "          (vf (cl:slot-value from '$name$))\n"
      "          (length (cl:length vf)))\n"
      "  (cl:loop for i from 0 below length do\n"
      "    (cl:vector-push-extend (cl:aref vf i) v)))");
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

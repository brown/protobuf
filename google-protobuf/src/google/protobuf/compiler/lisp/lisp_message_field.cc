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

#include <google/protobuf/compiler/lisp/lisp_message_field.h>
#include <google/protobuf/compiler/lisp/lisp_helpers.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>
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
                         map<string, string>* variables) {
  (*variables)["name"] = FieldName(descriptor);
  (*variables)["type"] = ClassName(descriptor->message_type(), false);
  (*variables)["index"] = SimpleItoa(descriptor->index());
  (*variables)["number"] = SimpleItoa(descriptor->number());
  (*variables)["classname"] = ClassName(FieldScope(descriptor), false);
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
      " :type (cl:or cl:null $type$))\n");
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
        "(cl:let ((s (octet-size (cl:slot-value self '$name$))))\n"
        "  (cl:incf size (cl:+ $tag_size$ s (varint:length32 s))))");
  } else if (descriptor_->type() == FieldDescriptor::TYPE_GROUP) {
    printer->Print(
        variables_,
        "(cl:incf size (cl:+ $tag_size$"
        " (octet-size (cl:slot-value self '$name$))))");
  } else {
    GOOGLE_LOG(FATAL) << "Invalid message type";
  }
}

void MessageFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // Message fields are lazily initialized the first time they are accessed.
  printer->Print(
      variables_,
      "(cl:defgeneric $name$ (proto))\n"
      "(cl:defmethod $name$ ((self $classname$))\n"
      "  (cl:let ((result (cl:slot-value self '$name$)))\n"
      "    (cl:when (cl:null result)\n"
      "      (cl:setf result (cl:make-instance '$type$))\n"
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
        " (cl:slot-value (cl:slot-value self '$name$) '%cached-size%)))\n"
        "(cl:setf index"
        " (serialize (cl:slot-value self '$name$) buffer index limit))");
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
        " (serialize (cl:slot-value self '$name$) buffer index limit))\n");
    printer->Print(
        "(cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $end_tag$))",
        "end_tag", SimpleItoa(end_tag));
  } else {
    GOOGLE_LOG(FATAL) << "Invalid message type";
  }
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
      "            :element-type '$type$\n"
      "            :fill-pointer 0 :adjustable cl:t)\n"
      " :type (cl:vector $type$))\n");
}

void RepeatedMessageFieldGenerator::GenerateClearingCode(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array 0 :element-type '$type$\n"
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
        "    (cl:let ((s (octet-size (cl:aref v i))))\n"
        "      (cl:incf size (cl:+ s (varint:length32 s))))))");
  } else if (descriptor_->type() == FieldDescriptor::TYPE_GROUP) {
    printer->Print(
        "    (cl:incf size (octet-size (cl:aref v i)))))");
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
        " buffer index limit (cl:slot-value (cl:aref v i) '%cached-size%)))\n"
        "     (cl:setf index (serialize (cl:aref v i) buffer index limit))))",
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
        "     (cl:setf index (serialize (cl:aref v i) buffer index limit))\n"
        "     (cl:setf index"
        " (varint:encode-uint32-carefully buffer index limit $end_tag$))))",
        "start_tag", SimpleItoa(start_tag),
        "end_tag", SimpleItoa(end_tag));
  } else {
    GOOGLE_LOG(FATAL) << "Invalid message type";
  }
}












void MessageFieldGenerator::
GenerateMergingCode(io::Printer* printer) const {
  printer->Print(variables_,
    "mutable_$name$()->$type$::MergeFrom(from.$name$());\n");
}

void MessageFieldGenerator::
GenerateMergeFromCodedStream(io::Printer* printer) const {
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    printer->Print(variables_,
      "DO_(::google::protobuf::internal::WireFormat::ReadMessageNoVirtual(\n"
      "     input, mutable_$name$()));\n");
  } else {
    printer->Print(variables_,
      "DO_(::google::protobuf::internal::WireFormat::ReadGroupNoVirtual("
        "$number$, input, mutable_$name$()));\n");
  }
}

// ===================================================================

void RepeatedMessageFieldGenerator::
GenerateMergingCode(io::Printer* printer) const {
  printer->Print(variables_, "$name$_.MergeFrom(from.$name$_);\n");
}

void RepeatedMessageFieldGenerator::
GenerateMergeFromCodedStream(io::Printer* printer) const {
  if (descriptor_->type() == FieldDescriptor::TYPE_MESSAGE) {
    printer->Print(variables_,
      "DO_(::google::protobuf::internal::WireFormat::ReadMessageNoVirtual(\n"
      "     input, add_$name$()));\n");
  } else {
    printer->Print(variables_,
      "DO_(::google::protobuf::internal::WireFormat::ReadGroupNoVirtual("
        "$number$, input, add_$name$()));\n");
  }
}


}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

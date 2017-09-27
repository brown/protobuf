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

#include "enum_field.h"
#include "helpers.h"
#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>
#include "strutil.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;

namespace {

// TODO(kenton):  Factor out a "SetCommonFieldVariables()" to get rid of
//   repeat code between this and the other field types.
void SetEnumVariables(const FieldDescriptor* descriptor,
                      std::map<string, string>* variables) {
  (*variables)["name"] = FieldName(descriptor);
  (*variables)["type"] = ClassName(descriptor->enum_type());
  (*variables)["default"] = DefaultValue(descriptor);
  (*variables)["package"] = FileLispPackage(descriptor->enum_type()->file());
  (*variables)["index"] = SimpleItoa(descriptor->index());
  (*variables)["number"] = SimpleItoa(descriptor->number());
//  (*variables)["classname"] = ClassName(FieldScope(descriptor));
  (*variables)["tag"] = SimpleItoa(WireFormat::MakeTag(descriptor));
  (*variables)["tag_size"] =
      SimpleItoa(WireFormat::TagSize(
                     descriptor->number(), descriptor->type()));
}

}  // namespace

EnumFieldGenerator::EnumFieldGenerator(const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetEnumVariables(descriptor, &variables_);
}

EnumFieldGenerator::~EnumFieldGenerator() {}

void EnumFieldGenerator::GenerateSlot(io::Printer* printer) const {
  printer->Print(
      variables_,
      "($name$\n"
      " :accessor $name$\n"
      " :initform $package$::$default$\n"
      " :type $package$::$type$)\n");
}

void EnumFieldGenerator::GenerateClearingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) $package$::$default$)");
}

void EnumFieldGenerator::GenerateOctetSize(io::Printer* printer) const {
  printer->Print(variables_, "(cl:incf size $tag_size$)\n");
  printer->Print(variables_, "(cl:incf size ");
  string size = OctetSize(descriptor_->type(), "(cl:slot-value self '$name$)");
  printer->Print(variables_, size.c_str());
  printer->Print(variables_, ")");
}

void EnumFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // The default accessor works fine for enum fields.
}

void EnumFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "(cl:setf index\n"
      " (varint:encode-uint64-carefully buffer index limit\n"
      "  (cl:ldb (cl:byte 64 0) (cl:slot-value self '$name$))))");
}

void EnumFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:multiple-value-bind (value new-index)\n"
      "    (varint:parse-int32-carefully buffer index limit)\n"
      "  ;; XXXXX: when valid, set field, else add to unknown fields\n"
      "  (cl:setf (cl:slot-value self '$name$) value)\n"
      "  (cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)\n"
      "  (cl:setf index new-index))");
}

void EnumFieldGenerator::GenerateMergingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) (cl:slot-value from '$name$))\n"
      "(cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)");
}

RepeatedEnumFieldGenerator::RepeatedEnumFieldGenerator(
    const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetEnumVariables(descriptor, &variables_);
}

RepeatedEnumFieldGenerator::~RepeatedEnumFieldGenerator() {}

void RepeatedEnumFieldGenerator::GenerateSlot(io::Printer* printer) const {
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

void RepeatedEnumFieldGenerator::GenerateClearingCode(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array\n"
      "          0\n"
      "          :element-type '$package$::$type$\n"
      "          :fill-pointer 0 :adjustable cl:t))");
}

void RepeatedEnumFieldGenerator::GenerateOctetSize(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:let* ((x (cl:slot-value self '$name$))\n"
      "          (length (cl:length x)))\n"
      "  (cl:incf size (cl:* $tag_size$ length))\n"
      "  (cl:dotimes (i length)\n"
      "    (cl:incf size"
      " (varint:length32 (cl:ldb (cl:byte 32 0) (cl:aref x i))))))");
}

void RepeatedEnumFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // The default accessor works fine for enum fields.
}

void RepeatedEnumFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:let* ((v (cl:slot-value self '$name$))\n"
      "          (length (cl:length v)))\n"
      "  (cl:loop for i from 0 below length do\n"
      "    (cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "    (cl:setf index (varint:encode-uint64-carefully buffer index limit\n"
      "                    (cl:ldb (cl:byte 64 0) (cl:aref v i))))))");
}

// XXXXXXXXXXXXXXXXXXXX C++ code for packed repeated enums adds the enum if
// valid, but otherwise throws it away.  The unpacked version adds the enum
// to unknown_fields if the message supports it.  Why the difference???

void RepeatedEnumFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  if (!descriptor_->options().packed()) {
    printer->Print(
        variables_,
        "(cl:multiple-value-bind (value new-index)\n"
        "    (varint:parse-int32-carefully buffer index limit)\n"
        "  ;; XXXXX: when valid, set field, else add to unknown fields\n"
        "  (cl:vector-push-extend value (cl:slot-value self '$name$))\n"
        "  (cl:setf index new-index))");
  } else {
    printer->Print(
        variables_,
        "(cl:multiple-value-bind (length new-index)\n"
        "    (varint:parse-uint32-carefully buffer index limit)\n"
        "  (cl:setf index new-index)\n"
        "  (cl:let ((end (cl:+ index length)))\n"
        "    (cl:loop while (cl:< index end) do\n"
        "      (cl:multiple-value-bind (value new-index)\n"
        "          (varint:parse-int32-carefully buffer index limit)\n"
        "        ;; XXXXX: when valid, set field, else add to unknown fields\n"
        "        (cl:vector-push-extend value (cl:slot-value self '$name$))\n"
        "        (cl:setf index new-index)))))");
  }
}

void RepeatedEnumFieldGenerator::GenerateMergingCode(
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

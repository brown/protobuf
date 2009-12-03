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

#include <google/protobuf/compiler/lisp/lisp_enum_field.h>
#include <google/protobuf/compiler/lisp/lisp_helpers.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>
#include <google/protobuf/stubs/strutil.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;

namespace {

// TODO(kenton):  Factor out a "SetCommonFieldVariables()" to get rid of
//   repeat code between this and the other field types.
void SetEnumVariables(const FieldDescriptor* descriptor,
                      map<string, string>* variables) {
  const EnumValueDescriptor* default_value = descriptor->default_value_enum();

  (*variables)["name"] = FieldName(descriptor);
  (*variables)["type"] = ClassName(descriptor->enum_type(), true);
  (*variables)["default"] = SimpleItoa(default_value->number());
//  (*variables)["index"] = SimpleItoa(descriptor->index());
  (*variables)["number"] = SimpleItoa(descriptor->number());
//  (*variables)["classname"] = ClassName(FieldScope(descriptor), false);
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
      " :initform $default$\n"
      " :type $type$)\n");
}

void EnumFieldGenerator::GenerateClearingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) $default$)");
}

void EnumFieldGenerator::GenerateOctetSize(io::Printer* printer) const {
  printer->Print(variables_, "(cl:incf size $tag_size$)\n");
  printer->Print(variables_, "(cl:incf size ");
  string size = OctetSize(descriptor_->type(), "(cl:slot-value self '$name$)");
  printer->Print(variables_, size.c_str());
  printer->Print(variables_, ")");
  printer->Print(
      variables_,
      "(cl:incf size (varint:length64 (cl:slot-value self '$name$)))\n");
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
      "  (base:int32-to-uint64 (cl:slot-value self '$name$))))");
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
      "            :element-type '$type$\n"
      "            :fill-pointer 0 :adjustable cl:t)\n"
      " :type (cl:vector $type$))\n");
}

void RepeatedEnumFieldGenerator::GenerateClearingCode(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array\n"
      "          0\n"
      "          :element-type '$type$\n"
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
      "    (varint:length32 (base:int32-to-uint32 (cl:aref x i)))))");
}

void RepeatedEnumFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // The default accessor works fine for enum fields.
}

void RepeatedEnumFieldGenerator::
GenerateSerializeWithCachedSizes(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "(cl:setf index\n"
      " (varint:encode-uint64-carefully buffer index limit\n"
      "  (base:int32-to-uint64 (aref x i)))");
}










void EnumFieldGenerator::
GenerateMergingCode(io::Printer* printer) const {
  printer->Print(variables_, "set_$name$(from.$name$());\n");
}

void EnumFieldGenerator::
GenerateMergeFromCodedStream(io::Printer* printer) const {
  printer->Print(variables_,
    "int value;\n"
    "DO_(::google::protobuf::internal::WireFormatLite::ReadEnum(input, &value));\n"
    "if ($type$_IsValid(value)) {\n"
    "  set_$name$(static_cast< $type$ >(value));\n"
    "} else {\n"
    "  mutable_unknown_fields()->AddField($number$)->add_varint(value);\n"
    "}\n");
}

// ===================================================================

void RepeatedEnumFieldGenerator::
GenerateMergingCode(io::Printer* printer) const {
  printer->Print(variables_, "$name$_.MergeFrom(from.$name$_);\n");
}

void RepeatedEnumFieldGenerator::
GenerateMergeFromCodedStream(io::Printer* printer) const {
  printer->Print(variables_,
    "int value;\n"
    "DO_(::google::protobuf::internal::WireFormatLite::ReadEnum(input, &value));\n"
    "if ($type$_IsValid(value)) {\n"
    "  add_$name$(static_cast< $type$ >(value));\n"
    "} else {\n"
    "  mutable_unknown_fields()->AddField($number$)->add_varint(value);\n"
    "}\n");
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

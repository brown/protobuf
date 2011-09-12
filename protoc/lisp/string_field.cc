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

#include "string_field.h"
#include "helpers.h"
#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>
#include <google/protobuf/descriptor.pb.h>
#include "strutil.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;

namespace {

// TODO(kenton):  Factor out a "SetCommonFieldVariables()" to get rid of
//   repeat code between this and the other field types.
void SetStringVariables(const FieldDescriptor* descriptor,
                        map<string, string>* variables) {
  (*variables)["name"] = FieldName(descriptor);
  (*variables)["type"] = PrimitiveTypeName(descriptor->cpp_type());
  (*variables)["defaultlength"]
      = SimpleItoa(descriptor->default_value_string().size());
  (*variables)["default"] = StringOctets(descriptor->default_value_string());
  (*variables)["index"] = SimpleItoa(descriptor->index());
  (*variables)["number"] = SimpleItoa(descriptor->number());
  (*variables)["classname"] = ClassName(FieldScope(descriptor), false);
  (*variables)["tag"] = SimpleItoa(WireFormat::MakeTag(descriptor));
  (*variables)["tag_size"]
      = SimpleItoa(WireFormat::TagSize(
                       descriptor->number(), descriptor->type()));
}

}  // namespace

// ===================================================================

StringFieldGenerator::StringFieldGenerator(const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetStringVariables(descriptor, &variables_);
}

StringFieldGenerator::~StringFieldGenerator() {}

void StringFieldGenerator::GenerateSlot(io::Printer* printer) const {
  printer->Print(
      variables_,
      "($name$\n"
//      " :accessor $name$\n"           XXXX: custom accessors
      " :initform (cl:make-array\n"
      "            $defaultlength$\n"
      "            :element-type '(cl:unsigned-byte 8)\n"
      "            :initial-contents '($default$))\n"
      " :type $type$)\n");
}

void StringFieldGenerator::GenerateClearingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array $defaultlength$"
      " :element-type '(cl:unsigned-byte 8)\n"
      "          :initial-contents '($default$)))");
}

void StringFieldGenerator::GenerateOctetSize(io::Printer* printer) const {
  printer->Print(variables_, "(cl:incf size $tag_size$)\n");
  printer->Print(variables_, "(cl:incf size ");
  string size = OctetSize(descriptor_->type(), "(cl:slot-value self '$name$)");
  printer->Print(variables_, size.c_str());
  printer->Print(variables_, ")");
}

void StringFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // XXXXXXXXXXXXXXX: the C++ code looks at
  // if (descriptor_->type() == FieldDescriptor::TYPE_BYTES)
  // and does some special things.

  // The string accessors convert octets to and from strings.
  printer->Print(
      variables_,
      "(cl:unless (cl:fboundp '$name$)\n"
      "  (cl:defgeneric $name$ (proto)))\n"
      "(cl:defmethod $name$ ((self $classname$))\n"
      "  (com.google.base:utf8-octets-to-string (cl:slot-value self '$name$)))\n"
      "\n"
      "(cl:export '$name$-octets)\n"
      "(cl:unless (cl:fboundp '$name$-octets)\n"
      "  (cl:defgeneric $name$-octets (proto)))\n"
      "(cl:defmethod $name$-octets ((self $classname$))\n"
      "  (cl:slot-value self '$name$))\n"
      "\n"
      "(cl:unless (cl:fboundp '(cl:setf $name$))\n"
      "  (cl:defgeneric (cl:setf $name$) (new-value proto)))\n"
      "(cl:defmethod (cl:setf $name$) (new-value (self $classname$))\n"
      "  (cl:etypecase new-value\n"
      "    ((cl:string)\n"
      "     (cl:setf (cl:slot-value self '$name$)\n"
      "              (com.google.base:string-to-utf8-octets new-value)))\n"
      "    ((com.google.base:octet-vector)\n"
      "     (cl:setf (cl:slot-value self '$name$) new-value)))\n"
      "  (cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)\n"
      "  new-value)\n");
}

void StringFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "(cl:setf index"
      " (wire-format:write-octets-carefully"
      " buffer index limit (cl:slot-value self '$name$)))");
}

void StringFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:multiple-value-bind (value new-index)\n"
      "    (wire-format:read-octets-carefully buffer index limit)\n"
      "  (cl:setf (cl:slot-value self '$name$) value)\n"
      "  (cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)\n"
      "  (cl:setf index new-index))");
}

void StringFieldGenerator::GenerateMergingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) (cl:slot-value from '$name$))\n"
      "(cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)");
}

RepeatedStringFieldGenerator::RepeatedStringFieldGenerator(
    const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetStringVariables(descriptor, &variables_);
}

RepeatedStringFieldGenerator::~RepeatedStringFieldGenerator() {}

void RepeatedStringFieldGenerator::GenerateSlot(io::Printer* printer) const {
  // XXXX: C++ code generator creates a _default_$name$_ member.
  // What is it used for?  Should we do the same?
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

void RepeatedStringFieldGenerator::GenerateClearingCode(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array\n"
      "          0\n"
      "          :element-type '$type$\n"
      "          :fill-pointer 0 :adjustable cl:t))");
}

void RepeatedStringFieldGenerator::GenerateOctetSize(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:let* ((x (cl:slot-value self '$name$))\n"
      "          (length (cl:length x)))\n"
      "  (cl:incf size (cl:* $tag_size$ length))\n"
      "  (cl:dotimes (i length)\n"
      "    (cl:incf size\n"
      "     (cl:let ((s (cl:length (cl:aref x i))))\n"
      "       (cl:+ s (varint:length32 s))))))");
}

void RepeatedStringFieldGenerator::GenerateAccessor(io::Printer* printer)
    const {
  // The default accessor works fine for repeated string fields.
}

void RepeatedStringFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:let* ((v (cl:slot-value self '$name$))\n"
      "          (length (cl:length v)))\n"
      "  (cl:loop for i from 0 below length do\n"
      "    (cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "    (cl:setf index"
      " (wire-format:write-octets-carefully"
      " buffer index limit (cl:aref v i)))))");
}

void RepeatedStringFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:multiple-value-bind (value new-index)\n"
      "    (wire-format:read-octets-carefully buffer index limit)\n"
      "  (cl:vector-push-extend value (cl:slot-value self '$name$))\n"
      "  (cl:setf index new-index))");
}

void RepeatedStringFieldGenerator::GenerateMergingCode(
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

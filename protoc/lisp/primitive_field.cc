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

#include "primitive_field.h"
#include "helpers.h"
#include <google/protobuf/io/printer.h>
#include <google/protobuf/wire_format.h>
#include "strutil.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;
using internal::WireFormatLite;

namespace {

// For encodings with fixed sizes, returns that size in bytes.  Otherwise
// returns -1.
int FixedSize(FieldDescriptor::Type type) {
  switch (type) {
    case FieldDescriptor::TYPE_INT32:
    case FieldDescriptor::TYPE_INT64:
    case FieldDescriptor::TYPE_UINT32:
    case FieldDescriptor::TYPE_UINT64:
    case FieldDescriptor::TYPE_SINT32:
    case FieldDescriptor::TYPE_SINT64:
      return -1;

    case FieldDescriptor::TYPE_FIXED32:
      return WireFormatLite::kFixed32Size;
    case FieldDescriptor::TYPE_FIXED64:
      return WireFormatLite::kFixed64Size;
    case FieldDescriptor::TYPE_SFIXED32:
      return WireFormatLite::kSFixed32Size;
    case FieldDescriptor::TYPE_SFIXED64:
      return WireFormatLite::kSFixed64Size;
    case FieldDescriptor::TYPE_FLOAT:
      return WireFormatLite::kFloatSize;
    case FieldDescriptor::TYPE_DOUBLE:
      return WireFormatLite::kDoubleSize;
    case FieldDescriptor::TYPE_BOOL:
      return WireFormatLite::kBoolSize;

    case FieldDescriptor::TYPE_ENUM:
    case FieldDescriptor::TYPE_STRING:
    case FieldDescriptor::TYPE_BYTES:
    case FieldDescriptor::TYPE_GROUP:
    case FieldDescriptor::TYPE_MESSAGE:
      break;

    // No default because we want the compiler to complain if any new
    // types are added.
  }
  GOOGLE_LOG(FATAL) << "Can't get here.";
  return -1;
}

string Serialize(const FieldDescriptor* field) {
  switch (field->type()) {
    case FieldDescriptor::TYPE_INT32:
      return ("(varint:encode-uint64-carefully"
              " buffer index limit (cl:ldb (cl:byte 64 0) $fetch$))");
    case FieldDescriptor::TYPE_INT64:
      return ("(varint:encode-uint64-carefully"
              " buffer index limit (cl:ldb (cl:byte 64 0) $fetch$))");
    case FieldDescriptor::TYPE_UINT32:
      return "(varint:encode-uint32-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_UINT64:
      return "(varint:encode-uint64-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_SINT32:
      return ("(varint:encode-uint32-carefully"
              " buffer index limit (wire-format:zig-zag-encode32 $fetch$))");
    case FieldDescriptor::TYPE_SINT64:
      return ("(varint:encode-uint64-carefully"
              " buffer index limit (wire-format:zig-zag-encode64 $fetch$))");
    case FieldDescriptor::TYPE_FIXED32:
      return "(wire-format:write-uint32-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_FIXED64:
      return "(wire-format:write-uint64-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_SFIXED32:
      return "(wire-format:write-int32-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_SFIXED64:
      return "(wire-format:write-int64-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_FLOAT:
      return ("(wire-format:write-single-float-carefully"
              " buffer index limit $fetch$)");
    case FieldDescriptor::TYPE_DOUBLE:
      return ("(wire-format:write-double-float-carefully"
              " buffer index limit $fetch$)");
    case FieldDescriptor::TYPE_BOOL:
      return ("(wire-format:write-boolean-carefully"
              " buffer index limit $fetch$)");

    case FieldDescriptor::TYPE_ENUM:
    case FieldDescriptor::TYPE_STRING:
    case FieldDescriptor::TYPE_BYTES:
    case FieldDescriptor::TYPE_GROUP:
    case FieldDescriptor::TYPE_MESSAGE:
      break;

    // No default because we want the compiler to complain if any new
    // types are added.
  }
  GOOGLE_LOG(FATAL) << "Can't get here.";
  return "";
}

string Deserialize(const FieldDescriptor* field) {
  switch (field->type()) {
    case FieldDescriptor::TYPE_INT32:
      return "(varint:parse-int32-carefully buffer index limit)";
    case FieldDescriptor::TYPE_INT64:
      return "(varint:parse-int64-carefully buffer index limit)";
    case FieldDescriptor::TYPE_UINT32:
      return "(varint:parse-uint32-carefully buffer index limit)";
    case FieldDescriptor::TYPE_UINT64:
      return "(varint:parse-uint64-carefully buffer index limit)";
    case FieldDescriptor::TYPE_SINT32:
      return ("(cl:multiple-value-bind (x new-index)\n"
              "    (varint:parse-uint32-carefully buffer index limit)\n"
              "  (cl:values (wire-format:zig-zag-decode32 x) new-index))");
    case FieldDescriptor::TYPE_SINT64:
      return ("(cl:multiple-value-bind (x new-index)\n"
              "    (varint:parse-uint64-carefully buffer index limit)\n"
              "  (cl:values (wire-format:zig-zag-decode64 x) new-index))");
    case FieldDescriptor::TYPE_FIXED32:
      return "(wire-format:read-uint32-carefully buffer index limit)";
    case FieldDescriptor::TYPE_FIXED64:
      return "(wire-format:read-uint64-carefully buffer index limit)";
    case FieldDescriptor::TYPE_SFIXED32:
      return "(wire-format:read-int32-carefully buffer index limit)";
    case FieldDescriptor::TYPE_SFIXED64:
      return "(wire-format:read-int64-carefully buffer index limit)";
    case FieldDescriptor::TYPE_FLOAT:
      return "(wire-format:read-single-float-carefully buffer index limit)";
    case FieldDescriptor::TYPE_DOUBLE:
      return "(wire-format:read-double-float-carefully buffer index limit)";
    case FieldDescriptor::TYPE_BOOL:
      return "(wire-format:read-boolean-carefully buffer index limit)";

    case FieldDescriptor::TYPE_ENUM:
    case FieldDescriptor::TYPE_STRING:
    case FieldDescriptor::TYPE_BYTES:
    case FieldDescriptor::TYPE_GROUP:
    case FieldDescriptor::TYPE_MESSAGE:
      break;

    // No default because we want the compiler to complain if any new
    // types are added.
  }
  GOOGLE_LOG(FATAL) << "Can't get here.";
  return "";
}

// TODO(kenton):  Factor out a "SetCommonFieldVariables()" to get rid of
//   repeat code between this and the other field types.
void SetPrimitiveVariables(const FieldDescriptor* descriptor,
                           map<string, string>* variables,
                           bool repeated) {
  (*variables)["name"] = FieldName(descriptor);
  (*variables)["type"] = PrimitiveTypeName(descriptor->cpp_type());
  (*variables)["default"] = DefaultValue(descriptor);
  (*variables)["index"] = SimpleItoa(descriptor->index());
//  (*variables)["number"] = SimpleItoa(descriptor->number());
//  (*variables)["classname"] = ClassName(FieldScope(descriptor), false);
  (*variables)["tag"] = SimpleItoa(WireFormat::MakeTag(descriptor));
  (*variables)["tag_size"] =
      SimpleItoa(WireFormat::TagSize(
                     descriptor->number(), descriptor->type()));
  string fetcher =
      (repeated
       ? "(cl:aref v i)"
       : StringReplace("(cl:slot-value self '$name$)",
                       "$name$", FieldName(descriptor), true));
  (*variables)["serialize"] =
      StringReplace(Serialize(descriptor), "$fetch$", fetcher, true);

  (*variables)["deserialize"] = Deserialize(descriptor);

  // XXXXXXXXXXXXXXXXXXXX this is only used in one place.  Could remove it.
  // Why doesn't non-repeated GenerateOctetSize() use this value????
  int fixed_size = FixedSize(descriptor->type());
  if (fixed_size != -1) {
    (*variables)["fixed_size"] = SimpleItoa(fixed_size);
  }
}

}  // namespace

PrimitiveFieldGenerator::PrimitiveFieldGenerator(
    const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetPrimitiveVariables(descriptor, &variables_, false);
}

PrimitiveFieldGenerator::~PrimitiveFieldGenerator() {}

void PrimitiveFieldGenerator::GenerateSlot(io::Printer* printer) const {
  printer->Print(
      variables_,
      "($name$\n"
      " :accessor $name$\n"
      " :initform $default$\n"
      " :type $type$)\n");
}

void PrimitiveFieldGenerator::GenerateClearingCode(io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) $default$)");
}

void PrimitiveFieldGenerator::GenerateOctetSize(io::Printer* printer) const {
  printer->Print(variables_, "(cl:incf size\n");
  printer->Print(variables_, "  (cl:+ $tag_size$ ");
  string size = OctetSize(descriptor_->type(), "(cl:slot-value self '$name$)");
  printer->Print(variables_, size.c_str());
  printer->Print(variables_, "))");
}

void PrimitiveFieldGenerator::GenerateAccessor(io::Printer* printer) const {
  // The default accessor works fine for primitive fields.
}

void PrimitiveFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "(cl:setf index $serialize$)");
}

void PrimitiveFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  printer->Print("(cl:multiple-value-bind (value new-index)\n");
  printer->Indent();
  printer->Indent();
  printer->Print(variables_, "$deserialize$\n");
  printer->Outdent();
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) value)\n"
      "(cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)\n"
      "(cl:setf index new-index))");
  printer->Outdent();
}

void PrimitiveFieldGenerator::GenerateMergingCode(io::Printer* printer) const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$) (cl:slot-value from '$name$))\n"
      "(cl:setf (cl:ldb (cl:byte 1 $index$)"
      " (cl:slot-value self '%has-bits%)) 1)");
}

RepeatedPrimitiveFieldGenerator::RepeatedPrimitiveFieldGenerator(
    const FieldDescriptor* descriptor)
    : descriptor_(descriptor) {
  SetPrimitiveVariables(descriptor, &variables_, true);
}

RepeatedPrimitiveFieldGenerator::~RepeatedPrimitiveFieldGenerator() {}

void RepeatedPrimitiveFieldGenerator::GenerateSlot(io::Printer* printer)
    const {
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

void RepeatedPrimitiveFieldGenerator::GenerateClearingCode(
    io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf (cl:slot-value self '$name$)\n"
      "         (cl:make-array 0 :element-type '$type$\n"
      "          :fill-pointer 0 :adjustable cl:t))");
}

void RepeatedPrimitiveFieldGenerator::GenerateOctetSize(io::Printer* printer)
    const {
  int fixed_size = FixedSize(descriptor_->type());
  if (fixed_size == -1) {
    printer->Print(
        variables_,
        "(cl:let* ((x (cl:slot-value self '$name$))\n"
        "          (length (cl:length x)))\n"
        "  (cl:incf size (cl:* $tag_size$ length))\n"
        "  (cl:dotimes (i length)\n"
        "    (cl:incf size\n"
        "     ");
    string size =
        OctetSize(descriptor_->type(),
                  "(cl:aref (cl:slot-value self '$name$) i)");
    printer->Print(variables_, size.c_str());
    printer->Print(variables_, ")))");
  } else {
    printer->Print(
        variables_,
        "(cl:incf size (cl:* (cl:+ $tag_size$ $fixed_size$)\n"
        "                 (cl:length (cl:slot-value self '$name$))))");
  }
}

void RepeatedPrimitiveFieldGenerator::GenerateAccessor(io::Printer* printer)
    const {
  // The default accessor works fine for primitive fields.
}

void RepeatedPrimitiveFieldGenerator::GenerateSerializeWithCachedSizes(
    io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:let* ((v (cl:slot-value self '$name$))\n"
      "          (length (cl:length v)))\n"
      "  (cl:loop for i from 0 below length do\n"
      "    (cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "    (cl:setf index $serialize$)))");
}

void RepeatedPrimitiveFieldGenerator::GenerateMergeFromArray(
    io::Printer* printer) const {
  if (!descriptor_->options().packed()) {
    printer->Print("(cl:multiple-value-bind (value new-index)\n");
    printer->Indent();
    printer->Indent();
    printer->Print(variables_, "$deserialize$\n");
    printer->Outdent();
    printer->Print(
        variables_,
        "  (cl:vector-push-extend value (cl:slot-value self '$name$))\n"
        "  (cl:setf index new-index))");
    printer->Outdent();
  } else {
    printer->Print(
        "(cl:multiple-value-bind (length new-index)\n"
        "    (varint:parse-uint32-carefully buffer index limit)\n"
        "  (cl:setf index new-index)\n"
        "  (cl:let ((end (cl:+ index length)))\n"
        "    (cl:loop while (cl:< index end) do\n");
    printer->Indent();
    printer->Indent();
    printer->Indent();
    printer->Print("(cl:multiple-value-bind (value new-index)\n");
    printer->Indent();
    printer->Indent();
    printer->Print(variables_, "$deserialize$\n");
    printer->Outdent();
    printer->Print(
        variables_,
        "  (cl:vector-push-extend value (cl:slot-value self '$name$))\n"
        "  (cl:setf index new-index)))))");
    printer->Outdent();
    printer->Outdent();
    printer->Outdent();
    printer->Outdent();
  }
}

void RepeatedPrimitiveFieldGenerator::GenerateMergingCode(
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

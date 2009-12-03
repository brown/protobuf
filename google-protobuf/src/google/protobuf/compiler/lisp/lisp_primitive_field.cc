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

#include <google/protobuf/compiler/lisp/lisp_primitive_field.h>
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

// For encodings with fixed sizes, returns that size in bytes.  Otherwise
// returns -1.
int FixedSize(FieldDescriptor::Type type) {
  switch (type) {
    case FieldDescriptor::TYPE_INT32: return -1;
    case FieldDescriptor::TYPE_INT64: return -1;
    case FieldDescriptor::TYPE_UINT32: return -1;
    case FieldDescriptor::TYPE_UINT64: return -1;
    case FieldDescriptor::TYPE_SINT32: return -1;
    case FieldDescriptor::TYPE_SINT64: return -1;
    case FieldDescriptor::TYPE_FIXED32: return WireFormatLite::kFixed32Size;
    case FieldDescriptor::TYPE_FIXED64: return WireFormatLite::kFixed64Size;
    case FieldDescriptor::TYPE_SFIXED32: return WireFormatLite::kSFixed32Size;
    case FieldDescriptor::TYPE_SFIXED64: return WireFormatLite::kSFixed64Size;
    case FieldDescriptor::TYPE_FLOAT: return WireFormatLite::kFloatSize;
    case FieldDescriptor::TYPE_DOUBLE: return WireFormatLite::kDoubleSize;
    case FieldDescriptor::TYPE_BOOL: return WireFormatLite::kBoolSize;

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

string DefaultValue(const FieldDescriptor* field) {
  switch (field->cpp_type()) {
    case FieldDescriptor::CPPTYPE_INT32:
      return SimpleItoa(field->default_value_int32());
    case FieldDescriptor::CPPTYPE_UINT32:
      return SimpleItoa(field->default_value_uint32());
    case FieldDescriptor::CPPTYPE_INT64:
      return SimpleItoa(field->default_value_int64());
    case FieldDescriptor::CPPTYPE_UINT64:
      return SimpleItoa(field->default_value_uint64());
    case FieldDescriptor::CPPTYPE_DOUBLE:
      return LispSimpleDtoa(field->default_value_double());
    case FieldDescriptor::CPPTYPE_FLOAT:
      return LispSimpleFtoa(field->default_value_float());
    case FieldDescriptor::CPPTYPE_BOOL:
      return field->default_value_bool() ? "cl:t" : "cl:nil";

    case FieldDescriptor::CPPTYPE_ENUM:
    case FieldDescriptor::CPPTYPE_STRING:
    case FieldDescriptor::CPPTYPE_MESSAGE:
      GOOGLE_LOG(FATAL) << "Shouldn't get here.";
      return "";
  }
  // Can't actually get here; make compiler happy.  We could add a default
  // case above but then we wouldn't get the nice compiler warning when a
  // new type is added.
  return "";
}

string Serialize(const FieldDescriptor* field) {
  switch (field->type()) {
    case FieldDescriptor::TYPE_INT32:
      return ("(varint:encode-uint64-carefully"
              " buffer index limit (base:int32-to-uint64 $fetch$))");
    case FieldDescriptor::TYPE_INT64:
      return ("(varint:encode-uint64-carefully"
              " buffer index limit (base:int64-to-uint64 $fetch$))");
    case FieldDescriptor::TYPE_UINT32:
      return "(varint:encode-uint32-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_UINT64:
      return "(varint:encode-uint64-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_SINT32:
      return ("(varint:encode-uint32-carefully"
              " buffer index limit (varint:zig-zag-encode32 $fetch$))");
    case FieldDescriptor::TYPE_SINT64:
      return ("(varint:encode-uint64-carefully"
              " buffer index limit (varint:zig-zag-encode64 $fetch$))");
    case FieldDescriptor::TYPE_FIXED32:
      return "(protocol:write-uint32-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_FIXED64:
      return "(protocol:write-uint64-carefully buffer index limit $fetch$)";
    case FieldDescriptor::TYPE_SFIXED32:
      return ("(protocol:write-int32-carefully buffer index limit $fetch$)");
    case FieldDescriptor::TYPE_SFIXED64:
      return ("(protocol:write-int64-carefully buffer index limit $fetch$)");
    case FieldDescriptor::TYPE_FLOAT:
      return ("(protocol:write-single-float-carefully"
              " buffer index limit $fetch$)");
    case FieldDescriptor::TYPE_DOUBLE:
      return ("(protocol:write-double-float-carefully"
              " buffer index limit $fetch$)");
    case FieldDescriptor::TYPE_BOOL:
      return ("(protocol:write-boolean-carefully"
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
    io::Printer* printer)
    const {
  printer->Print(
      variables_,
      "(cl:setf index"
      " (varint:encode-uint32-carefully buffer index limit $tag$))\n"
      "(cl:setf index $serialize$)");
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









void PrimitiveFieldGenerator::
GenerateMergingCode(io::Printer* printer) const {
  printer->Print(variables_, "set_$name$(from.$name$());\n");
}

void PrimitiveFieldGenerator::
GenerateMergeFromCodedStream(io::Printer* printer) const {
  printer->Print(variables_,
    ";;DO_(::google::protobuf::internal::WireFormat::Read$declared_type$(\n"
    ";;      input, &$name$_));\n"
    ";;_set_bit($index$);\n");
}

// ===================================================================

void RepeatedPrimitiveFieldGenerator::
GenerateMergingCode(io::Printer* printer) const {
  printer->Print(variables_, "$name$_.MergeFrom(from.$name$_);\n");
}

void RepeatedPrimitiveFieldGenerator::
GenerateMergeFromCodedStream(io::Printer* printer) const {
  printer->Print(variables_,
    ";;$type$ value;\n"
    ";;DO_(::google::protobuf::internal::WireFormat::Read$declared_type$(input, &value));\n"
    ";;add_$name$(value);\n");
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

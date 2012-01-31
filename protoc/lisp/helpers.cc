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

#include <vector>
#include "hash.h"

#include "helpers.h"
#include <google/protobuf/stubs/common.h>
#include "strutil.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

namespace {

string UnderscoresToHyphens(const string& name) {
  return StringReplace(name, "_", "-", true);
}

string DotsToUnderscores(const string& name) {
  return StringReplace(name, ".", "_", true);
}

string DotsToColons(const string& name) {
  return StringReplace(name, ".", "::", true);
}

}  // namespace


string StripProto(const string& filename) {
  if (HasSuffixString(filename, ".protodevel")) {
    return StripSuffixString(filename, ".protodevel");
  } else {
    return StripSuffixString(filename, ".proto");
  }
}

string LispifyName(const string& proto_name) {
  string result = UnderscoresToHyphens(proto_name);
  LowerString(&result);
  return result;
}

string ClassName(const EnumDescriptor* enum_descriptor, bool qualified) {
  // qualified must be FALSE  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  if (enum_descriptor->containing_type() == NULL) {
    return LispifyName(enum_descriptor->name());
  } else {
    string result = ClassName(enum_descriptor->containing_type(), qualified);
    result += '-';
    result += enum_descriptor->name();
    return LispifyName(result);
  }
}

string ClassName(const Descriptor* descriptor, bool qualified) {
  // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX qualified must be false


  // Find "outer", the descriptor of the top-level message in which
  // "descriptor" is embedded.
  const Descriptor* outer = descriptor;
  while (outer->containing_type() != NULL) {
    outer = outer->containing_type();
  }
  const string& outer_name = outer->full_name();
  string inner_name = descriptor->full_name().substr(outer_name.size());
  return LispifyName(outer->name() + DotsToUnderscores(inner_name));
}

const char* PrimitiveTypeName(FieldDescriptor::CppType type) {
  switch (type) {
    case FieldDescriptor::CPPTYPE_INT32:
      return "(cl:signed-byte 32)";
    case FieldDescriptor::CPPTYPE_INT64:
      return "(cl:signed-byte 64)";
    case FieldDescriptor::CPPTYPE_UINT32:
      return "(cl:unsigned-byte 32)";
    case FieldDescriptor::CPPTYPE_UINT64:
      return "(cl:unsigned-byte 64)";
    case FieldDescriptor::CPPTYPE_DOUBLE:
      return "cl:double-float";
    case FieldDescriptor::CPPTYPE_FLOAT:
      return "cl:single-float";
    case FieldDescriptor::CPPTYPE_BOOL:
      return "cl:boolean";
    case FieldDescriptor::CPPTYPE_ENUM:
      return "(cl:unsigned-byte 32)";
    case FieldDescriptor::CPPTYPE_STRING:
      return "(cl:simple-array (cl:unsigned-byte 8) (cl:*))";
    case FieldDescriptor::CPPTYPE_MESSAGE:
      return NULL;

    // No default because we want the compiler to complain if any new
    // CppTypes are added.
  }

  GOOGLE_LOG(FATAL) << "Can't get here.";
  return NULL;
}

string FieldName(const FieldDescriptor* field) {
  return LispifyName(UnderscoresToHyphens(field->name()));
}

string StringOctets(const string string_default) {
  string octets;
  int default_length = string_default.size();

  for (int i = 0; i < default_length; ++i) {
    int octet = string_default[i] & 0xff;
    octets += SimpleItoa(octet);
    if (i != default_length - 1) {
      octets += " ";
    }
  }
  return octets;
}

string LispSimpleFtoa(float value) {
  string c_result = SimpleDtoa(value);
  if (c_result == "inf") {
    return "+single-float-positive-infinity+";
  } else if (c_result == "-inf") {
    return "+single-float-negative-infinity+";
  } else if (c_result == "nan") {
    return "+single-float-nan+";
  }

  string::size_type pos = c_result.find("e", 0);
  if (pos != string::npos) {
    c_result.replace(pos, 1, "f");
    return c_result;
  }

  return c_result + "f0";
}

string LispSimpleDtoa(double value) {
  string c_result = SimpleDtoa(value);
  if (c_result == "inf") {
    return "+double-float-positive-infinity+";
  } else if (c_result == "-inf") {
    return "+double-float-negative-infinity+";
  } else if (c_result == "nan") {
    return "+double-float-nan+";
  }

  string::size_type pos = c_result.find("e", 0);
  if (pos != string::npos) {
    c_result.replace(pos, 1, "d");
    return c_result;
  }

  return c_result + "d0";
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
      return ("#.+"
              + ClassName(field->enum_type(), false)
              + "-"
              + LispifyName(field->default_value_enum()->name())
              + "+");

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

string OctetSize(FieldDescriptor::Type type, string reference) {
  switch (type) {
    case FieldDescriptor::TYPE_INT32:
    case FieldDescriptor::TYPE_ENUM:
      // XXXXXXXXXXXXXXXXXXXX: see coded_stream.h for how this can be coded
      // more efficiently: VarintSize32SignExtended().  The result is 10 if
      // value is negative, else it's length32(value).
      return "(varint:length64 (cl:ldb (cl:byte 64 0) " + reference + "))";
    case FieldDescriptor::TYPE_INT64:
      return "(varint:length64 (cl:ldb (cl:byte 64 0) " + reference + "))";
    case FieldDescriptor::TYPE_UINT32:
      return "(varint:length32 " + reference + ")";
    case FieldDescriptor::TYPE_UINT64:
      return "(varint:length64 " + reference + ")";
    case FieldDescriptor::TYPE_SINT32:
      return ("(varint:length32 (wire-format:zig-zag-encode32 "
              + reference + "))");
    case FieldDescriptor::TYPE_SINT64:
      return ("(varint:length64 (wire-format:zig-zag-encode64 "
              + reference + "))");

    case FieldDescriptor::TYPE_FIXED32:
    case FieldDescriptor::TYPE_SFIXED32:
    case FieldDescriptor::TYPE_FLOAT:
      return "4";

    case FieldDescriptor::TYPE_FIXED64:
    case FieldDescriptor::TYPE_SFIXED64:
    case FieldDescriptor::TYPE_DOUBLE:
      return "8";

    case FieldDescriptor::TYPE_BOOL:
      return "1";

    case FieldDescriptor::TYPE_STRING:
    case FieldDescriptor::TYPE_BYTES:
      return ("(cl:let ((s (cl:length " + reference + ")))\n"
              "  (cl:+ s (varint:length32 s)))");

    case FieldDescriptor::TYPE_GROUP:
      return "(octet-size " + reference + ")";
    case FieldDescriptor::TYPE_MESSAGE:
      return ("(cl:let ((s (octet-size " + reference + ")))\n"
              "  (cl:+ s (varint:length32 s)))");

    // No default because we want the compiler to complain if any new
    // types are added.
  }
  GOOGLE_LOG(FATAL) << "Can't get here.";
  return "";
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

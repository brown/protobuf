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

#include "helpers.h"

#include <vector>

#include <google/protobuf/stubs/common.h>
#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/stubs/strutil.h>

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

string FileLispPackage(const FileDescriptor* file) {
  if (file->package().size() != 0) {
    return LispifyName(file->package());
  } else if (file->options().has_java_package()) {
    return LispifyName(file->options().java_package());
  } else {
    return "protocol-buffer";
  }
}

const int unknown = 0;
const int lower = 1;
const int upper = 2;

string HyphenateStudlyCaps(const string& name) {
  int state = unknown;
  string result;

  for (int i = 0; i < name.size(); ++i) {
    result.append(1, name[i]);
    switch (state) {
      case unknown:
        if (isalpha(name[i])) {
          if (isupper(name[i])) {
            state = upper;
          } else if (islower(name[i])) {
            state = lower;
          }
        }
        break;
      case lower:
        if (i < name.size() - 1) {
          // We can look ahead one character.
          if (! isalpha(name[i + 1])) {
            state = unknown;
          } else if (isupper(name[i + 1])) {
            result.append(1, '-');
            state = upper;
          }
        }
        break;
      case upper:
        if (i < name.size() - 2) {
          // We can look two characters ahead.
          if (! isalpha(name[i + 1])) {
            state = unknown;
          } else if (islower(name[i + 1])) {
            state = lower;
          } else if (isalpha(name[i + 2]) && islower(name[i + 2])) {
            // Next character is upper, following character is lower.
            result.append(1, '-');
            result.append(1, name[++i]);
            state = lower;
          }
        }
        break;
    }
  }
  return result;
}

string LispifyName(const string& proto_name) {
  string result = UnderscoresToHyphens(proto_name);
  result = HyphenateStudlyCaps(result);
  LowerString(&result);
  return result;
}

string ClassName(const EnumDescriptor* enum_descriptor) {
  if (enum_descriptor->containing_type() == NULL) {
    // The enum type is defined at top-level in the file.
    return LispifyName(enum_descriptor->name());
  } else {
    // The enum type is embedded in a message filed definition.
    string result = ClassName(enum_descriptor->containing_type());
    result += '-';
    result += LispifyName(enum_descriptor->name());
    return result;
  }
}

string ClassName(const Descriptor* descriptor) {
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

const char* PrimitiveTypeName(const FieldDescriptor* field) {
  switch (field->cpp_type()) {
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
      if (field->type() == FieldDescriptor::TYPE_BYTES) {
        return "(cl:simple-array (cl:unsigned-byte 8) (cl:*))";
      } else {
        return "pb::%sf%";
      }
    case FieldDescriptor::CPPTYPE_MESSAGE:
      return NULL;

    // No default because we want the compiler to complain if any new
    // CppTypes are added.
  }

  GOOGLE_LOG(FATAL) << "Can't get here.";
  return NULL;
}

string FieldName(const FieldDescriptor* field) {
  // Groups are hacky:  The name of the field is just the lower-cased name
  // of the group type.  In Java, though, we would like to retain the original
  // capitalization of the type name.
  if (field->type() == FieldDescriptor::TYPE_GROUP) {
    return LispifyName(field->message_type()->name());
  } else {
    return LispifyName(field->name());
  }
}

string StringOctets(const string str) {
  string octets;
  int str_length = str.size();

  for (int i = 0; i < str_length; ++i) {
    int octet = str[i] & 0xff;
    octets += SimpleItoa(octet);
    if (i != str_length - 1) {
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

string LispEscapeString(string str) {
  string lisp;

  lisp.append(1, '"');
  for (int i = 0; i < str.size(); i++) {
    if (str[i] == '"') {
      lisp.append(1, '\\');
      lisp.append(1, '"');
    } else {
      lisp.append(1, str[i]);
    }
  }
  lisp.append(1, '"');
  return lisp;
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

    case FieldDescriptor::CPPTYPE_STRING:
      if (field->type() == FieldDescriptor::TYPE_BYTES) {
        if (field->has_default_value()) {
          return ("(cl:make-array "
                  + SimpleItoa(field->default_value_string().size())
                  + " :element-type '(cl:unsigned-byte 8) :initial-contents '("
                  + StringOctets(field->default_value_string())
                  + "))");
        } else {
          return "(cl:make-array 0 :element-type '(cl:unsigned-byte 8))";
        }
      } else {
        if (field->has_default_value()) {
          return "(pb:string-field " + LispEscapeString(field->default_value_string()) + ")";
        } else {
          return "(pb:string-field \"\")";
        }
      }

    case FieldDescriptor::CPPTYPE_ENUM:
      return ("+"
              + ClassName(field->enum_type())
              + "-"
              + LispifyName(field->default_value_enum()->name())
              + "+");

    case FieldDescriptor::CPPTYPE_MESSAGE:
      return StringOctets(field->default_value_string());
    // No default because we want the compiler to complain if any new types are
    // added.
  }

  GOOGLE_LOG(FATAL) << "Shouldn't get here.";
  return "";
}

string OctetSize(FieldDescriptor::Type type, string reference) {
  switch (type) {
    case FieldDescriptor::TYPE_INT32:
    case FieldDescriptor::TYPE_ENUM:
      // XXXX: The C++ function VarintSize32SignExtended() in coded_stream.h
      // does the legnth calculation using bit manipulation.
      return "(varint:length-int32 " + reference + ")";
    case FieldDescriptor::TYPE_INT64:
      return "(varint:length-uint64 (cl:ldb (cl:byte 64 0) " + reference + "))";
    case FieldDescriptor::TYPE_UINT32:
      return "(varint:length-uint32 " + reference + ")";
    case FieldDescriptor::TYPE_UINT64:
      return "(varint:length-uint64 " + reference + ")";
    case FieldDescriptor::TYPE_SINT32:
      return ("(varint:length-uint32 (wire-format:zig-zag-encode32 "
              + reference + "))");
    case FieldDescriptor::TYPE_SINT64:
      return ("(varint:length-uint64 (wire-format:zig-zag-encode64 "
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
      return ("(cl:let ((s (pb::%utf8-string-length% " + reference + ")))\n"
              "  (cl:+ s (varint:length-uint32 s)))");
    case FieldDescriptor::TYPE_BYTES:
      return ("(cl:let ((s (cl:length " + reference + ")))\n"
              "  (cl:+ s (varint:length-uint32 s)))");

    case FieldDescriptor::TYPE_GROUP:
      return "(pb:octet-size " + reference + ")";
    case FieldDescriptor::TYPE_MESSAGE:
      return ("(cl:let ((s (pb:octet-size " + reference + ")))\n"
              "  (cl:+ s (varint:length-uint32 s)))");

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

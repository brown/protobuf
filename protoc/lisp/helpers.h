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

#ifndef GOOGLE_PROTOBUF_COMPILER_LISP_HELPERS_H__
#define GOOGLE_PROTOBUF_COMPILER_LISP_HELPERS_H__

#include <string>
#include <google/protobuf/descriptor.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

// Strips ".proto" or ".protodevel" from the end of a filename.
string StripProto(const string& filename);

// Returns the file's Lisp package name.
string FileLispPackage(const FileDescriptor* file);

// Converts underscores to hyphens and characters to lower case.
string LispifyName(const string& proto_name);

string ClassName(const Descriptor* descriptor);
string ClassName(const EnumDescriptor* enum_descriptor);

// XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX fix comments

// Get the (unqualified) name that should be used for this field.  People
// should be using lowercase-with-underscores style for proto field names
// and this function replaces the underscores with hyphens.
string FieldName(const FieldDescriptor* field);


// XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX comment these

const char* PrimitiveTypeName(const FieldDescriptor* field);
string StringOctets(const string string_default);
string LispSimpleFtoa(float value);
string LispSimpleDtoa(double value);
string DefaultValue(const FieldDescriptor* field);
string OctetSize(FieldDescriptor::Type type, string reference);

// Returns the scope where the field was defined (for extensions, this is
// different from the message type to which the field applies).
inline const Descriptor* FieldScope(const FieldDescriptor* field) {
  return field->is_extension() ?
    field->extension_scope() : field->containing_type();
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

#endif  // GOOGLE_PROTOBUF_COMPILER_LISP_HELPERS_H__

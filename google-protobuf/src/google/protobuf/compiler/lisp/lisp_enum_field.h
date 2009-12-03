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

#ifndef GOOGLE_PROTOBUF_COMPILER_LISP_ENUM_FIELD_H__
#define GOOGLE_PROTOBUF_COMPILER_LISP_ENUM_FIELD_H__

#include <map>
#include <string>
#include <google/protobuf/compiler/lisp/lisp_field.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

class EnumFieldGenerator : public FieldGenerator {
 public:
  explicit EnumFieldGenerator(const FieldDescriptor* descriptor);
  ~EnumFieldGenerator();

  void GenerateSlot(io::Printer* printer) const;
  void GenerateClearingCode(io::Printer* printer) const;
  void GenerateOctetSize(io::Printer* printer) const;
  void GenerateAccessor(io::Printer* printer) const;
  void GenerateSerializeWithCachedSizes(io::Printer* printer) const;
  void GenerateMergeFromCodedStream(io::Printer* printer) const;






  // implements FieldGenerator ---------------------------------------
  void GenerateMergingCode(io::Printer* printer) const;


 private:
  const FieldDescriptor* descriptor_;
  map<string, string> variables_;

  GOOGLE_DISALLOW_EVIL_CONSTRUCTORS(EnumFieldGenerator);
};

class RepeatedEnumFieldGenerator : public FieldGenerator {
 public:
  explicit RepeatedEnumFieldGenerator(const FieldDescriptor* descriptor);
  ~RepeatedEnumFieldGenerator();

  void GenerateSlot(io::Printer* printer) const;
  void GenerateClearingCode(io::Printer* printer) const;
  void GenerateOctetSize(io::Printer* printer) const;
  void GenerateAccessor(io::Printer* printer) const;




  // implements FieldGenerator ---------------------------------------
  void GenerateMergingCode(io::Printer* printer) const;
  void GenerateMergeFromCodedStream(io::Printer* printer) const;
  void GenerateSerializeWithCachedSizes(io::Printer* printer) const;


 private:
  const FieldDescriptor* descriptor_;
  map<string, string> variables_;

  GOOGLE_DISALLOW_EVIL_CONSTRUCTORS(RepeatedEnumFieldGenerator);
};

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

#endif  // GOOGLE_PROTOBUF_COMPILER_LISP_ENUM_FIELD_H__

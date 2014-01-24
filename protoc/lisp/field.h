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

#ifndef GOOGLE_PROTOBUF_COMPILER_LISP_FIELD_H__
#define GOOGLE_PROTOBUF_COMPILER_LISP_FIELD_H__

#include <memory>
#include <google/protobuf/stubs/common.h>
#include <google/protobuf/descriptor.h>

namespace google {

namespace protobuf {
  namespace io {
    class Printer;             // printer.h
  }
}

namespace protobuf {
namespace compiler {
namespace lisp {

class FieldGenerator {
 public:
  FieldGenerator() {}
  virtual ~FieldGenerator();

  // Generate the slot definition for the field.
  virtual void GenerateSlot(io::Printer* printer) const = 0;

  // Generate code to clear the field.  The code is used in the
  // method that clears just this field as well as the method that clears the
  // whole message.
  virtual void GenerateClearingCode(io::Printer* printer) const = 0;

  // Generate code to compute the serialized size of this field for a
  // message's IS-INITIALIZED function.
  virtual void GenerateOctetSize(io::Printer* printer) const = 0;

  // Generate slot accessors for the field.
  virtual void GenerateAccessor(io::Printer* printer) const = 0;

  // Generate code to serialize this field.
  virtual void GenerateSerializeWithCachedSizes(io::Printer* printer)
      const = 0;

  // Generate code to de-serialized this field.
  virtual void GenerateMergeFromArray(io::Printer* printer) const = 0;

  // Generate code that merges the contents of another field for the same
  // type contained in a message called "from" into this field.
  virtual void GenerateMergingCode(io::Printer* printer) const = 0;

 private:
  GOOGLE_DISALLOW_EVIL_CONSTRUCTORS(FieldGenerator);
};

// Convenience class which constructs FieldGenerators for a Descriptor.
class FieldGeneratorMap {
 public:
  explicit FieldGeneratorMap(const Descriptor* descriptor);
  ~FieldGeneratorMap();

  const FieldGenerator& get(const FieldDescriptor* field) const;

 private:
  const Descriptor* descriptor_;
  std::unique_ptr<std::unique_ptr<FieldGenerator> []> field_generators_;

  static FieldGenerator* MakeGenerator(const FieldDescriptor* field);

  GOOGLE_DISALLOW_EVIL_CONSTRUCTORS(FieldGeneratorMap);
};

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf

}  // namespace google

#endif  // GOOGLE_PROTOBUF_COMPILER_LISP_FIELD_H__

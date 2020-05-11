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

#include "field.h"

#include <memory>

#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/stubs/common.h>
#include "enum_field.h"
#include "message_field.h"
#include "primitive_field.h"
#include "string_field.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

FieldGenerator::~FieldGenerator() {}

void FieldGenerator::GenerateMergeFromArrayWithPacking(io::Printer* printer) const {
  // Reaching here indicates a bug. Cases are:
  //   - This FieldGenerator should support packing, but this method should be
  //     overridden.
  //   - This FieldGenerator doesn't support packing, and this method should
  //     never have been called.
  GOOGLE_LOG(FATAL) << "GenerateMergeFromCodedStreamWithPacking() "
             << "called on field generator that does not support packing.";

}

FieldGeneratorMap::FieldGeneratorMap(const Descriptor* descriptor)
    : descriptor_(descriptor),
      field_generators_(
          new std::unique_ptr<FieldGenerator>[descriptor->field_count()]) {
  // Construct all the FieldGenerators.
  for (int i = 0; i < descriptor->field_count(); i++) {
    field_generators_[i].reset(MakeGenerator(descriptor->field(i)));
  }
}

FieldGenerator* FieldGeneratorMap::MakeGenerator(
    const FieldDescriptor* field) {
  if (field->is_repeated()) {
    switch (field->cpp_type()) {
      case FieldDescriptor::CPPTYPE_MESSAGE:
        return new RepeatedMessageFieldGenerator(field);
      case FieldDescriptor::CPPTYPE_STRING:
        return new RepeatedStringFieldGenerator(field);
      case FieldDescriptor::CPPTYPE_ENUM:
        return new RepeatedEnumFieldGenerator(field);
      default:
        return new RepeatedPrimitiveFieldGenerator(field);
    }
  } else {
    switch (field->cpp_type()) {
      case FieldDescriptor::CPPTYPE_MESSAGE:
        return new MessageFieldGenerator(field);
      case FieldDescriptor::CPPTYPE_STRING:
        return new StringFieldGenerator(field);
      case FieldDescriptor::CPPTYPE_ENUM:
        return new EnumFieldGenerator(field);
      default:
        return new PrimitiveFieldGenerator(field);
    }
  }
}

FieldGeneratorMap::~FieldGeneratorMap() {}

const FieldGenerator& FieldGeneratorMap::get(
    const FieldDescriptor* field) const {
  GOOGLE_CHECK_EQ(field->containing_type(), descriptor_);
  return *field_generators_[field->index()];
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

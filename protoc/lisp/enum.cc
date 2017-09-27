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

#include <set>
#include <map>

#include "enum.h"
#include "helpers.h"
#include <google/protobuf/io/printer.h>
#include "strutil.h"

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

EnumGenerator::EnumGenerator(const EnumDescriptor* descriptor)
    : descriptor_(descriptor),
      classname_(ClassName(descriptor)) {
}

EnumGenerator::~EnumGenerator() {}

void EnumGenerator::GenerateConstants(io::Printer* printer) {
    std::map<string, string> vars;
  vars["classname"] = classname_;

  const EnumValueDescriptor* min_value = descriptor_->value(0);
  const EnumValueDescriptor* max_value = descriptor_->value(0);

  for (int i = 0; i < descriptor_->value_count(); i++) {
    vars["name"] = LispifyName(descriptor_->value(i)->name());
    vars["number"] = SimpleItoa(descriptor_->value(i)->number());

    printer->Print(
        vars,
        "(cl:defconstant +$classname$-$name$+ $number$)\n"
        "(cl:export '+$classname$-$name$+)\n");

    if (descriptor_->value(i)->number() < min_value->number()) {
      min_value = descriptor_->value(i);
    }
    if (descriptor_->value(i)->number() > max_value->number()) {
      max_value = descriptor_->value(i);
    }
  }
  printer->Print("\n");

  vars["min_name"] = LispifyName(min_value->name());
  vars["max_name"] = LispifyName(max_value->name());
  printer->Print(
      vars,
      "(cl:defconstant +minimum-$classname$+ +$classname$-$min_name$+)\n"
      "(cl:export '+minimum-$classname$+)\n"
      "(cl:defconstant +maximum-$classname$+ +$classname$-$max_name$+)\n"
      "(cl:export '+maximum-$classname$+)\n"
      "\n");
}

void EnumGenerator::GenerateDefType(io::Printer* printer) {
  // Multiple enums may have the same numeric value.  Use a set to ensure we
  // do not output the same number more than once.
    std::set<int> numbers;
  for (int j = 0; j < descriptor_->value_count(); j++) {
    const EnumValueDescriptor* value = descriptor_->value(j);
    numbers.insert(value->number());
  }

  printer->Print("(cl:deftype $classname$ () '(cl:member ",
                 "classname", classname_);
  for (std::set<int>::iterator iter = numbers.begin();
       iter != numbers.end();
       ++iter) {
    if (iter != numbers.begin()) {
      printer->Print(" ");
    }
    printer->Print("$number$", "number", SimpleItoa(*iter));
  }
  printer->Print("))\n");
  printer->Print("(cl:export '$classname$)\n\n", "classname", classname_);
}

// void EnumGenerator::GeneratePackageExports(io::Printer* printer) {
//   map<string, string> vars;
//   vars["classname"] = classname_;

//   const EnumValueDescriptor* min_value = descriptor_->value(0);
//   const EnumValueDescriptor* max_value = descriptor_->value(0);

//   printer->Print(vars, "(:export #:$classname$\n");
//   printer->Indent();

//   for (int i = 0; i < descriptor_->value_count(); i++) {
//     vars["name"] = LispifyName(descriptor_->value(i)->name());
//     vars["number"] = SimpleItoa(descriptor_->value(i)->number());

//     printer->Print(vars, "#:+$classname$-$name$+\n");

//     if (descriptor_->value(i)->number() < min_value->number()) {
//       min_value = descriptor_->value(i);
//     }
//     if (descriptor_->value(i)->number() > max_value->number()) {
//       max_value = descriptor_->value(i);
//     }
//   }

//   vars["min_name"] = LispifyName(min_value->name());
//   vars["max_name"] = LispifyName(max_value->name());
//   printer->Print(
//       vars,
//       "#:+minimum-$classname$+\n"
//       "#:+maximum-$classname$+)");

//   printer->Outdent();
// }

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

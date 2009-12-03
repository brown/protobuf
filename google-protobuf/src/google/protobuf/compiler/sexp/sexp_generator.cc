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

#include <google/protobuf/compiler/sexp/sexp_generator.h>

#include <vector>
#include <utility>

//#include <google/protobuf/compiler/cpp/cpp_file.h>
//#include <google/protobuf/compiler/cpp/cpp_helpers.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/stubs/strutil.h>

#include <google/protobuf/descriptor.h> // for FileDescriptor


namespace google {
namespace protobuf {
namespace compiler {
namespace sexp {

SexpGenerator::SexpGenerator() {}
SexpGenerator::~SexpGenerator() {}


// Returns a copy of file_name with any trailing ".protodevel" or ".proto"
// suffix stripped.
// TODO(brown): Unify with copy in compiler/cpp/internal/helpers.cc.
static string StripProto(const string& file_name) {
  const char* suffix = HasSuffixString(file_name, ".protodevel")
      ? ".protodevel" : ".proto";
  return StripSuffixString(file_name, suffix);
}

void SexpGenerator::PrintHeader(io::Printer* printer,
                                const string& sexp_file_name,
                                const FileDescriptor* file) const {
  printer->Print("\n;;;;    $sexp_file$\n\n", "sexp_file", sexp_file_name);
  printer->Print("(cl:in-package #:protocol-buffer)\n\n");
  printer->Print("(define-protobuf-package $name$)\n\n",
                 "name", file->package());

  // XXXX: skipping option information  FileOptions in descriptor.proto
}

void SexpGenerator::PrintDescriptor(io::Printer* printer,
                                    const Descriptor& message_descriptor)
    const {

//   printer->Print("(define-protobuf $descriptor_name$\n",
//                  "descriptor_name",
//                  ModuleLevelDescriptorName(message_descriptor));

// (define-protobuf time-protocol
//   ((g :repeated :type :nested-group :tag 1
//       ((v1 :required :type int32 :tag 2)
//        (v2 :required :type int32 :tag 3)))
//    (debug :repeated :type octet-vector :tag 4)))


//   PrintNestedDescriptors(message_descriptor);

//   printer_->Print("\n");
//   printer_->Print("$descriptor_name$ = descriptor.Descriptor(\n",
//                   "descriptor_name",
//                   ModuleLevelDescriptorName(message_descriptor));
//   printer_->Indent();
//   map<string, string> m;
//   m["name"] = message_descriptor.name();
//   m["full_name"] = message_descriptor.full_name();
//   m["filename"] = message_descriptor.file()->name();
//   const char required_function_arguments[] =
//       "name='$name$',\n"
//       "full_name='$full_name$',\n"
//       "filename='$filename$',\n"
//       "containing_type=None,\n";  // TODO(robinson): Implement containing_type.
//   printer_->Print(m, required_function_arguments);
//   PrintFieldsInDescriptor(message_descriptor);
//   PrintExtensionsInDescriptor(message_descriptor);
//   // TODO(robinson): implement printing of nested_types.
//   printer_->Print("nested_types=[],  # TODO(robinson): Implement.\n");
//   printer_->Print("enum_types=[\n");
//   printer_->Indent();
//   for (int i = 0; i < message_descriptor.enum_type_count(); ++i) {
//     const string descriptor_name = ModuleLevelDescriptorName(
//         *message_descriptor.enum_type(i));
//     printer_->Print(descriptor_name.c_str());
//     printer_->Print(",\n");
//   }
//   printer_->Outdent();
//   printer_->Print("],\n");
//   string options_string;
//   message_descriptor.options().SerializeToString(&options_string);
//   printer_->Print(
//       "options=$options_value$",
//       "options_value", OptionsValue("MessageOptions", options_string));
//   printer_->Outdent();
//   printer_->Print(")\n");
}

void SexpGenerator::PrintMessageDescriptors(io::Printer* printer,
                                           const FileDescriptor* file) const {
  for (int i = 0; i < file->message_type_count(); ++i) {
    PrintDescriptor(printer, *file->message_type(i));
    printer->Print("\n");
  }
}

bool SexpGenerator::Generate(const FileDescriptor* file,
                             const string& parameter,
                             OutputDirectory* output_directory,
                             string* error) const {
  string file_name = StripProto(file->name());
  file_name += ".lisp";

  scoped_ptr<io::ZeroCopyOutputStream>
      output(output_directory->Open(file_name + "_sexp.lisp"));
  GOOGLE_CHECK(output.get());
  io::Printer printer(output.get(), '$');

  PrintHeader(&printer, file_name, file);



//   string basename = StripProto(file->name());
//   FileGenerator file_generator(file);

//   // Generate s-expression representation.
//   scoped_ptr<io::ZeroCopyOutputStream> output(
//       output_directory->Open(basename + ".lisp"));
//   io::Printer printer(output.get(), '$');
//   file_generator.GenerateSexp(&printer);


  return !printer.failed();
}

}  // namespace sexp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

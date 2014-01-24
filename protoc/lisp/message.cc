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

#include <algorithm>
#include "hash.h"
#include <memory>
#include "message.h"
#include "enum.h"
//#include <google/protobuf/compiler/lisp/lisp_extension.h>
#include "helpers.h"
#include "strutil.h"
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/wire_format.h>
#include <google/protobuf/descriptor.pb.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;
using internal::WireFormatLite;

namespace {

void PrintFieldComment(io::Printer* printer, const FieldDescriptor* field) {
  // Print the field's proto-syntax definition as a comment.  We don't want to
  // print group bodies so we cut off after the first line.
  string def = field->DebugString();
  printer->Print(";; $def$\n",
                 "def", def.substr(0, def.find_first_of('\n')));
}

struct FieldOrderingByNumber {
  inline bool operator()(const FieldDescriptor* a,
                         const FieldDescriptor* b) const {
    return a->number() < b->number();
  }
};

// Sort the fields of the given Descriptor by number into a new[]'d array
// and return it.
const FieldDescriptor** SortFieldsByNumber(const Descriptor* descriptor) {
  const FieldDescriptor** fields =
    new const FieldDescriptor*[descriptor->field_count()];
  for (int i = 0; i < descriptor->field_count(); i++) {
    fields[i] = descriptor->field(i);
  }
  sort(fields, fields + descriptor->field_count(),
       FieldOrderingByNumber());
  return fields;
}

// Functor for sorting extension ranges by their "start" field number.
struct ExtensionRangeSorter {
  bool operator()(const Descriptor::ExtensionRange* left,
                  const Descriptor::ExtensionRange* right) const {
    return left->start < right->start;
  }
};

// Returns true if the message type has any required fields.  If it doesn't,
// we can optimize out calls to its IsInitialized() method.
//
// already_seen is used to avoid checking the same type multiple times
// (and also to protect against recursion).
static bool HasRequiredFields(
    const Descriptor* type,
    hash_set<const Descriptor*>* already_seen) {
  if (already_seen->count(type) > 0) {
    // Since the first occurrence of a required field causes the whole
    // function to return true, we can assume that if the type is already
    // in the cache it didn't have any required fields.
    return false;
  }
  already_seen->insert(type);

  // If the type has extensions, an extension with message type could contain
  // required fields, so we have to be conservative and assume such an
  // extension exists.
  if (type->extension_range_count() > 0) {
    return true;
  }

  for (int i = 0; i < type->field_count(); i++) {
    const FieldDescriptor* field = type->field(i);
    if (field->is_required()) {
      return true;
    }
    if (field->cpp_type() == FieldDescriptor::CPPTYPE_MESSAGE) {
      if (HasRequiredFields(field->message_type(), already_seen)) {
        return true;
      }
    }
  }

  return false;
}

static bool HasRequiredFields(const Descriptor* type) {
  hash_set<const Descriptor*> already_seen;
  return HasRequiredFields(type, &already_seen);
}

}

// ===================================================================

MessageGenerator::MessageGenerator(const Descriptor* descriptor)
    : descriptor_(descriptor),
      classname_(ClassName(descriptor)),
      //    dllexport_decl_(dllexport_decl),
      field_generators_(descriptor),
      nested_generators_(new std::unique_ptr<
          MessageGenerator>[descriptor->nested_type_count()]),
      enum_generators_(
          new std::unique_ptr<EnumGenerator>[descriptor->enum_type_count()])
      //  ,  extension_generators_(new scoped_ptr<ExtensionGenerator>[
      //      descriptor->extension_count()])
{

  for (int i = 0; i < descriptor->nested_type_count(); i++) {
    nested_generators_[i].reset(
      new MessageGenerator(descriptor->nested_type(i)));
  }

  for (int i = 0; i < descriptor->enum_type_count(); i++) {
    enum_generators_[i].reset(
      new EnumGenerator(descriptor->enum_type(i)));
  }

  for (int i = 0; i < descriptor->extension_count(); i++) {
    // extension_generators_[i].reset(
    //   new ExtensionGenerator(descriptor->extension(i)));
  }
}

MessageGenerator::~MessageGenerator() {}

void MessageGenerator::GenerateEnumDefinitions(io::Printer* printer) {
  for (int i = 0; i < descriptor_->nested_type_count(); i++) {
    nested_generators_[i]->GenerateEnumDefinitions(printer);
  }

  for (int i = 0; i < descriptor_->enum_type_count(); i++) {
    enum_generators_[i]->GenerateDefType(printer);
    enum_generators_[i]->GenerateConstants(printer);
  }
}

// void MessageGenerator::GeneratePackageExports(io::Printer* printer) {
//   // Nested classes
//   for (int i = 0; i < descriptor_->nested_type_count(); i++) {
//     nested_generators_[i]->GeneratePackageExports(printer);
//     printer->Print("\n");
//   }
//   // Enums
//   for (int i = 0; i < descriptor_->enum_type_count(); i++) {
//     enum_generators_[i]->GeneratePackageExports(printer);
//     printer->Print("\n");
//   }

//   map<string, string> vars;
//   vars["classname"] = classname_;

//   printer->Print(vars, "(:export #:$classname$\n");
//   printer->Indent();

//   for (int i = 0; i < descriptor_->field_count(); i++) {
//     const FieldDescriptor* field = descriptor_->field(i);
//     vars["name"] = FieldName(field);

//     // Export the field accessor symbols
//     printer->Print(vars, "#:$name$ #:has-$name$ #:clear-$name$\n");
//   }

//   printer->Print(")");
//   printer->Outdent();
// }

void MessageGenerator::GenerateClassDefinition(io::Printer* printer) {

  // Generate class definitions of all nested classes.
  for (int i = 0; i < descriptor_->nested_type_count(); i++) {
    nested_generators_[i]->GenerateClassDefinition(printer);
    printer->Print("\n");
  }

  map<string, string> vars;
  vars["classname"] = classname_;
  vars["field_count"] = SimpleItoa(descriptor_->field_count());

  printer->Print(vars, "(cl:defclass $classname$ (pb:protocol-buffer)\n");
  printer->Indent();
  printer->Print("(\n");

  // Generate field slot definitions.
  for (int i = 0; i < descriptor_->field_count(); i++) {
    field_generators_.get(descriptor_->field(i)).GenerateSlot(printer);
  }

  if (descriptor_->field_count() > 0) {
    printer->Print(
        vars,
        "(%has-bits%\n"
        " :accessor %has-bits%\n"
        " :initform 0\n"
        " :type (cl:unsigned-byte $field_count$))\n");
  }
  printer->Print(
      vars,
      "(pb::%cached-size%\n"
      " :initform 0\n"
      " :type (cl:integer 0 #.(cl:1- cl:array-dimension-limit)))\n");
  printer->Print("))\n");
  printer->Outdent();
  printer->Print("\n");

  // Export the class.
  printer->Print(
      vars,
      "(cl:export '$classname$)\n"
      "\n");

  // Generate additional field methods.

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);
    vars["name"] = FieldName(field);
    vars["index"] = SimpleItoa(field->index());

    // Export the field accessor symbol.
    printer->Print(
        vars,
        "(cl:export '$name$)\n"
        "\n");

    if (!field->is_repeated()) {
      // Singular field.

      // Generate custom accessors, if necessary.  Currently, only string
      // and message slots use non-standard accessors.

      field_generators_.get(field).GenerateAccessor(printer);
      printer->Print("\n");

      // Generate an after method that sets the right bit in %HAS-BITS%
      // whenever the field is modified.
      printer->Print(
          vars,
          "(cl:defmethod (cl:setf $name$) :after (x (self $classname$))\n"
          "  (cl:declare (cl:ignore x))\n"
          "  (cl:setf"
          " (cl:ldb (cl:byte 1 $index$) (cl:slot-value self '%has-bits%))"
          " 1))\n"
          "\n");

      // Generate HAS method.
      printer->Print(
          vars,
          "(cl:unless (cl:fboundp 'has-$name$)\n"
          "  (cl:defgeneric has-$name$ (proto)))\n"
          "(cl:defmethod has-$name$ ((self $classname$))\n"
          "  (cl:logbitp $index$ (cl:slot-value self '%has-bits%)))\n"
          "(cl:export 'has-$name$)\n"
          "\n");
    }

    // Generate CLEAR method.
    printer->Print(
        vars,
        "(cl:unless (cl:fboundp 'clear-$name$)\n"
        "  (cl:defgeneric clear-$name$ (proto)))\n"
        "(cl:defmethod clear-$name$ ((self $classname$))\n");
    printer->Indent();
    field_generators_.get(field).GenerateClearingCode(printer);
    printer->Print("\n");
    if (!field->is_repeated()) {
      // Singular field.
      printer->Print(
          vars,
          "(cl:setf"
          " (cl:ldb (cl:byte 1 $index$) (cl:slot-value self '%has-bits%))"
          " 0)\n");
    }
    printer->Print("(cl:values))\n");
    printer->Outdent();

    printer->Print(
        vars,
        "(cl:export 'clear-$name$)\n"
        "\n");
  }
  printer->Print("\n");
}

void MessageGenerator::GenerateClassMethods(io::Printer* printer) {
  for (int i = 0; i < descriptor_->nested_type_count(); i++) {
    nested_generators_[i]->GenerateClassMethods(printer);
    printer->Print("\n");
    printer->Print("\n");
  }
  GeneratePrintObject(printer);
  printer->Print("\n");

  GenerateClear(printer);
  printer->Print("\n");

  GenerateIsInitialized(printer);
  printer->Print("\n");

  GenerateOctetSize(printer);
  printer->Print("\n");

  GenerateSerializeWithCachedSizes(printer);
  printer->Print("\n");

  GenerateMergeFromArray(printer);
  printer->Print("\n");

  GenerateMergeFromMessage(printer);
  printer->Print("\n");
}

void MessageGenerator::GeneratePrintObject(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod cl:print-object ((self $classname$) stream)\n"
      "  (cl:pprint-logical-block (stream cl:nil)\n"
      "    (cl:print-unreadable-object (self stream :type cl:t :identity cl:t)\n",
      "classname", classname_);
  printer->Indent();
  printer->Indent();
  printer->Indent();

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (!field->is_repeated()) {
      printer->Print(
          "(cl:when (cl:logbitp $index$ (cl:slot-value self '%has-bits%))\n",
          "index", SimpleItoa(field->index()));
      printer->Indent();
    }
    // Use the getter so that string protobuf fields are output as Lisp
    // strings, not as arrays of octets.
    printer->Print(
        "(cl:format stream \" ~_$name$: ~s\" ($name$ self))",
        "name", FieldName(field));
    if (!field->is_repeated()) {
      printer->Print(")");
      printer->Outdent();
    }
    printer->Print("\n");
  }

  printer->Print("))\n");
  printer->Outdent();
  printer->Outdent();
  printer->Print("(cl:values))\n");
  printer->Outdent();
}

void MessageGenerator::GenerateClear(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod pb:clear ((self $classname$))\n",
      "classname", classname_);
  printer->Indent();

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (!field->is_repeated()) {
      map<string, string> vars;
      vars["index"] = SimpleItoa(field->index());

      // XXXXXXXXXXXXXXXXXXXX: This logic may be wrong for embedded Lisp
      // messages.  Instead of checkint %HAS-BITS%, we should compare the
      // message fields to NIL, since they are NIL until set.  Should we do
      // something similar for string fields??

      // It's faster to just overwrite primitive types, but we should
      // only clear strings and messages if they were set.
      // TODO(kenton):  Let the CppFieldGenerator decide this somehow.
      bool should_check_bit =
        field->cpp_type() == FieldDescriptor::CPPTYPE_MESSAGE ||
        field->cpp_type() == FieldDescriptor::CPPTYPE_STRING;

      if (should_check_bit) {
        printer->Print(
            vars,
            "(cl:when"
            " (cl:logbitp $index$ (cl:slot-value self '%has-bits%))\n");
        printer->Indent();
      }

      field_generators_.get(field).GenerateClearingCode(printer);

      if (should_check_bit) {
        printer->Outdent();
        printer->Print(")");
      }
      printer->Print("\n");
    }
  }

  // Repeated fields don't use %HAS-BITS%, so clear them in a separate pass.

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (field->is_repeated()) {
      field_generators_.get(field).GenerateClearingCode(printer);
      printer->Print("\n");
    }
  }

  printer->Print("(cl:setf (cl:slot-value self '%has-bits%) 0)\n"
                 "(cl:values))\n");
  printer->Outdent();
}

void MessageGenerator::GenerateIsInitialized(io::Printer* printer) {
  printer->Print("(cl:defmethod pb:is-initialized ((self $classname$))\n",
                 "classname", classname_);
  printer->Indent();

  // Verify that all required fields are set.

  bool has_required_field = false;
  string mask;
  for (int i = 0; i < descriptor_->field_count(); ++i) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (field->is_required()) {
      mask = "1" + mask;
      has_required_field = true;
    } else {
      mask = "0" + mask;
    }
  }
  if (has_required_field) {
    printer->Print(
        "(cl:when (cl:/= (cl:logand (cl:slot-value self '%has-bits%)\n"
        "                           #b$mask$)\n"
        "                #b$mask$)\n"
        "  (cl:return-from pb:is-initialized cl:nil))\n",
        "mask", mask);
  }

  // Verify that all embedded messages are initialized.

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);
    if (field->cpp_type() == FieldDescriptor::CPPTYPE_MESSAGE
        && HasRequiredFields(field->message_type())) {
      if (field->is_repeated()) {
        printer->Print(
            "(cl:let* ((x (cl:slot-value self '$name$))\n"
            "          (length (cl:length x)))\n"
            "  (cl:dotimes (i length)\n"
            "    (cl:unless (pb:is-initialized (cl:aref x i))\n"
            "      (cl:return-from pb:is-initialized cl:nil))))\n",
            "name", FieldName(field));
      } else {
        // XXXXXXXXXXXXXXXXXXXX: not sure what's going on here with
        // required vs. optional fields
        // Maybe the C++ code hardwires has-XXX to true for required fields.
        printer->Print(
            "(cl:when (cl:logbitp $index$ (cl:slot-value self '%has-bits%))\n"
            "  (cl:unless (pb:is-initialized (cl:slot-value self '$name$))\n"
            "    (cl:return-from pb:is-initialized cl:nil)))\n",
          "index", SimpleItoa(field->index()),
          "name", FieldName(field));
      }
    }
  }

  printer->Print("cl:t)\n");
  printer->Outdent();
}

void MessageGenerator::GenerateOctetSize(io::Printer* printer) {
  printer->Print("(cl:defmethod pb:octet-size ((self $classname$))\n",
                 "classname", classname_);
  printer->Indent();

  // XXXXXXXXXXXXXXXXXXXX  previous Lisp code does:
  // (assert (pb:is-initialized self))

  printer->Print("(cl:let ((size 0))\n");
  printer->Indent();

  // XXXXXXXXXXXXXXXXXXXX This code generates checks of %HAS-BITS% for
  // required fields, which seems odd.  The previous Lisp code did not, but
  // also required that the protocol buffer be initialized -- all required
  // fields present.

  // Add sizes of all non-repeated fields.

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);

    // XXXXXXXXXXXXXXXXXXXX: Why do we always generate the WHEN HAS-BITS check?
    // Required fields should always be present.
    if (!field->is_repeated()) {
      PrintFieldComment(printer, field);
      printer->Print(
          "(cl:when (cl:logbitp $index$ (cl:slot-value self '%has-bits%))\n",
          "index", SimpleItoa(field->index()));
      printer->Indent();
      field_generators_.get(field).GenerateOctetSize(printer);
      printer->Print(")\n");
      printer->Outdent();
    }
  }

  // Repeated fields don't use %HAS-BITS%, so count them in a separate pass.

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (field->is_repeated()) {
      PrintFieldComment(printer, field);
      field_generators_.get(field).GenerateOctetSize(printer);
      printer->Print("\n");
    }
  }

  // We update _cached_size_ even though this is a const method.  In theory,
  // this is not thread-compatible, because concurrent writes have undefined
  // results.  In practice, since any concurrent writes will be writing the
  // exact same value, it works on all common processors.  In a future version
  // of C++, _cached_size_ should be made into an atomic<int>.
  // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX: Protect update with lock ???
  printer->Print(
      "(cl:setf (cl:slot-value self 'pb::%cached-size%) size)\n"
      "size))\n");
  printer->Outdent();
  printer->Outdent();
}

void MessageGenerator::GenerateSerializeOneField(
    io::Printer* printer,
    const FieldDescriptor* field) {
  PrintFieldComment(printer, field);

  if (!field->is_repeated()) {
    printer->Print(
        "(cl:when (cl:logbitp $index$ (cl:slot-value self '%has-bits%))\n",
        "index", SimpleItoa(field->index()));
    printer->Indent();
    field_generators_.get(field).GenerateSerializeWithCachedSizes(printer);
    printer->Outdent();
    printer->Print(")\n");
  } else {
    field_generators_.get(field).GenerateSerializeWithCachedSizes(printer);
    printer->Print("\n");
  }
}

void MessageGenerator::GenerateSerializeOneExtensionRange(
    io::Printer* printer, const Descriptor::ExtensionRange* range) {
  map<string, string> vars;
  vars["start"] = SimpleItoa(range->start);
  vars["end"] = SimpleItoa(range->end);
  // XXXXXXXXXX
  printer->Print(vars,
    ";// Extension range [$start$, $end$)\n"
    ";DO_(_extensions_.SerializeWithCachedSizes(\n"
    ";    $start$, $end$, *this, output));\n\n");
}

void MessageGenerator::GenerateSerializeWithCachedSizes(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod pb:serialize ((self $classname$) buffer index limit)\n"
      "  (cl:declare (cl:type com.google.base:octet-vector buffer)\n"
      "              (cl:type com.google.base:vector-index index limit)\n"
      "              (cl:ignorable buffer limit))\n",
      "classname", classname_);
  printer->Indent();

  std::unique_ptr<const FieldDescriptor * []> ordered_fields(
      SortFieldsByNumber(descriptor_));

  vector<const Descriptor::ExtensionRange*> sorted_extensions;
  for (int i = 0; i < descriptor_->extension_range_count(); ++i) {
    sorted_extensions.push_back(descriptor_->extension_range(i));
  }
  sort(sorted_extensions.begin(),
       sorted_extensions.end(),
       ExtensionRangeSorter());

  // Merge the fields and the extension ranges, both sorted by field number.
  int i, j;
  for (i = 0, j = 0;
       i < descriptor_->field_count() || j < sorted_extensions.size();
       ) {
    if (i == descriptor_->field_count()) {
      GenerateSerializeOneExtensionRange(printer, sorted_extensions[j++]);
    } else if (j == sorted_extensions.size()) {
      GenerateSerializeOneField(printer, ordered_fields[i++]);
    } else if (ordered_fields[i]->number() < sorted_extensions[j]->start) {
      GenerateSerializeOneField(printer, ordered_fields[i++]);
    } else {
      GenerateSerializeOneExtensionRange(printer, sorted_extensions[j++]);
    }
  }

#if 0
  printer->Print("if (!unknown_fields().empty()) {\n");
  printer->Indent();
  if (descriptor_->options().message_set_wire_format()) {
    // XXXXXXXXXX
    printer->Print(
      ";DO_(::google::protobuf::internal::WireFormat::SerializeUnknownMessageSetItems(\n"
      ";    unknown_fields(), output));\n");
  } else {
    // XXXXXXXXXX
    printer->Print(
      ";DO_(::google::protobuf::internal::WireFormat::SerializeUnknownFields(\n"
      ";    unknown_fields(), output));\n");
  }
  printer->Outdent();
  printer->Print(
    "}\n"
    "return true;\n");
#endif

  printer->Print("index)");
  printer->Outdent();
  printer->Print("\n");
}

void MessageGenerator::GenerateMergeFromArray(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod pb:merge-from-array"
      " ((self $classname$) buffer start limit)\n"
      "  (cl:declare (cl:type com.google.base:octet-vector buffer)\n"
      "              (cl:type com.google.base:vector-index start limit))\n"
      "  (cl:do ((index start index))\n"
      "      ((cl:>= index limit) index)\n"
      "    (cl:declare (cl:type com.google.base:vector-index index))\n"
      "    (cl:multiple-value-bind (tag new-index)\n"
      "        (varint:parse-uint32-carefully buffer index limit)\n"
      "      (cl:setf index new-index)\n"
      "      (cl:case tag\n",
      "classname", classname_);
  printer->Indent();
  printer->Indent();
  printer->Indent();
  printer->Indent();

  std::unique_ptr<const FieldDescriptor * []> ordered_fields(
      SortFieldsByNumber(descriptor_));

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = ordered_fields[i];

    PrintFieldComment(printer, field);
    printer->Print(
        "(($tag$)\n",
        "tag", SimpleItoa(WireFormat::MakeTag(field)));
    printer->Indent();
    field_generators_.get(field).GenerateMergeFromArray(printer);
    printer->Print(")");
    printer->Outdent();
    printer->Print("\n");
  }

  printer->Print(
      "(cl:t\n");
  // XXXXXXXXXX: We never verify the field number part of the tag.  The old C++
  // doesn't either ... more recent C++ code creates switch cases for group end
  // tags and we should too.
  printer->Indent();
  printer->Print(
      "(cl:when (cl:= (cl:logand tag 7) $end_group$)\n"
      "  (cl:return-from pb:merge-from-array index))\n",
      "end_group", SimpleItoa(WireFormatLite::WIRETYPE_END_GROUP));

  // Skip over unknown fields.  XXXXXXXXXXXXXXXXXXXX: They should be collected
  // into a set of unknown fields.
  printer->Print(
      "(cl:setf index (wire-format:skip-field buffer index limit tag))");
  printer->Print(")))))\n");
  printer->Outdent();
  printer->Outdent();
  printer->Outdent();
  printer->Outdent();
  printer->Outdent();
}

void MessageGenerator::GenerateMergeFromMessage(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod pb:merge-from-message"
      " ((self $classname$) (from $classname$))\n",
      "classname", classname_);
  printer->Indent();

  // Merge Repeated fields. These fields do not require a
  // check as we can simply iterate over them.
  for (int i = 0; i < descriptor_->field_count(); ++i) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (field->is_repeated()) {
      field_generators_.get(field).GenerateMergingCode(printer);
      printer->Print("\n");
    }
  }

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (!field->is_repeated()) {
      map<string, string> vars;
      vars["index"] = SimpleItoa(field->index());

      // XXXXXXXXXXXXXXXXXXXX: This logic may be wrong for embedded Lisp
      // messages.  Instead of checkint %HAS-BITS%, we should compare the
      // message fields to NIL, since they are NIL until set.  Should we do
      // something similar for string fields??
      printer->Print(
          vars,
          "(cl:when (cl:logbitp $index$ (cl:slot-value from '%has-bits%))\n");
      printer->Indent();

      field_generators_.get(field).GenerateMergingCode(printer);

      printer->Outdent();
      printer->Print(")\n");
    }
  }
  printer->Outdent();
  printer->Print(")\n");

  // XXXXXXXXXXXXXXX extensions
//  if (descriptor_->extension_range_count() > 0) {
//    printer->Print("_extensions_.MergeFrom(from._extensions_);\n");
//  }

  // XXXXXXXXXXXXXXXXXXXX unknown fields
//  printer->Print(
//    "mutable_unknown_fields()->MergeFrom(from.unknown_fields());\n");
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

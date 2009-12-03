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
#include <google/protobuf/stubs/hash.h>
#include <google/protobuf/compiler/lisp/lisp_message.h>
#include <google/protobuf/compiler/lisp/lisp_enum.h>
//#include <google/protobuf/compiler/lisp/lisp_extension.h>
#include <google/protobuf/compiler/lisp/lisp_helpers.h>
#include <google/protobuf/stubs/strutil.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/wire_format.h>
#include <google/protobuf/descriptor.pb.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace lisp {

using internal::WireFormat;

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

const char* kWireTypeNames[] = {
  "VARINT",
  "FIXED64",
  "LENGTH_DELIMITED",
  "START_GROUP",
  "END_GROUP",
  "FIXED32",
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
    classname_(ClassName(descriptor, false)),
//    dllexport_decl_(dllexport_decl),
    field_generators_(descriptor),
    nested_generators_(new scoped_ptr<MessageGenerator>[
      descriptor->nested_type_count()]),
    enum_generators_(new scoped_ptr<EnumGenerator>[
      descriptor->enum_type_count()])
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

void MessageGenerator::GenerateClassDefinition(io::Printer* printer) {

  // Generate class definitions of all nested classes.
  for (int i = 0; i < descriptor_->nested_type_count(); i++) {
    nested_generators_[i]->GenerateClassDefinition(printer);
    printer->Print("\n");
  }

  map<string, string> vars;
  vars["classname"] = classname_;
  vars["field_count"] = SimpleItoa(descriptor_->field_count());

  printer->Print(vars, "(cl:defclass $classname$ (protocol-buffer)\n");
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
      "(%cached-size%\n"
      " :accessor %cached-size%\n"
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
          "  (cl:setf"
          " (cl:ldb (cl:byte 1 $index$) (cl:slot-value self '%has-bits%))"
          " 1))\n"
          "\n");

      // Generate HAS method.
      printer->Print(
          vars,
          "(cl:defgeneric has-$name$ (proto))\n"
          "(cl:defmethod has-$name$ ((self $classname$))\n"
          "  (cl:logbitp $index$ (cl:slot-value self '%has-bits%)))\n"
          "(cl:export 'has-$name$)\n"
          "\n");
    }

    // Generate CLEAR method.
    printer->Print(
        vars,
        "(cl:defgeneric clear-$name$ (proto))\n"
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

#if 0
  GenerateMergeFromCodedStream(printer);
  printer->Print("\n");
#endif

  // GenerateMergeFrom(printer);
  // printer->Print("\n");
}

void MessageGenerator::GeneratePrintObject(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod cl:print-object ((self $classname$) stream)\n"
      "  (cl:print-unreadable-object"
      " (self stream :type cl:t :identity cl:t)\n",
      "classname", classname_);
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
    printer->Print(
        "(cl:format stream \"$name$: ~s \" (cl:slot-value self '$name$))",
        "name", FieldName(field));
    if (!field->is_repeated()) {
      printer->Print(")");
      printer->Outdent();
    }
    printer->Print("\n");
  }

  printer->Print(")\n");
  printer->Outdent();
  printer->Print("(cl:values))\n");
  printer->Outdent();
}

void MessageGenerator::GenerateClear(io::Printer* printer) {
  printer->Print(
      "(cl:defmethod clear ((self $classname$))\n",
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
  printer->Print("(cl:defmethod is-initialized ((self $classname$))\n",
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
        "  (cl:return-from is-initialized cl:nil))\n",
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
            "    (cl:unless (is-initialized (cl:aref x i))\n"
            "      (cl:return-from is-initialized cl:nil))))\n",
            "name", FieldName(field));
      } else {
        // XXXXXXXXXXXXXXXXXXXX: not sure what's going on here with
        // required vs. optional fields
        // Maybe the C++ code hardwires has-XXX to true for required fields.
        printer->Print(
            "(cl:when (cl:logbitp $index$ (cl:slot-value self '%has-bits%))\n"
            "  (cl:unless (is-initialized (cl:slot-value self '$name$))\n"
            "    (cl:return-from is-initialized cl:nil)))\n",
          "index", SimpleItoa(field->index()),
          "name", FieldName(field));
      }
    }
  }

  printer->Print("cl:t)\n");
  printer->Outdent();
}

void MessageGenerator::GenerateOctetSize(io::Printer* printer) {
  printer->Print("(cl:defmethod octet-size ((self $classname$))\n",
                 "classname", classname_);
  printer->Indent();

  // XXXXXXXXXXXXXXXXXXXX  previous Lisp code does:
  // (assert (is-initialized self))

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
      "(cl:setf (cl:slot-value self '%cached-size%) size)\n"
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
      "(cl:defmethod serialize ((self $classname$) buffer index limit)\n"
      "  (cl:declare (cl:type base:octet-vector buffer)\n"
      "              (cl:type base:octet-vector-index index limit)\n"
      "              (cl:ignorable buffer limit))\n",
      "classname", classname_);
  printer->Indent();

  scoped_array<const FieldDescriptor*> ordered_fields(
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

void MessageGenerator::GenerateMergeFromCodedStream(io::Printer* printer) {
  if (descriptor_->options().message_set_wire_format()) {
    // For message_set_wire_format, we don't generate a parser, for two
    // reasons:
    // - WireFormat already needs to special-case this, and we'd like to
    //   avoid having multiple implementations of MessageSet wire format
    //   lying around the code base.
    // - All fields are extensions, and extension parsing falls back to
    //   reflection anyway, so it wouldn't be any faster.
    printer->Print(
      "bool $classname$::MergePartialFromCodedStream(\n"
      "    ::google::protobuf::io::CodedInputStream* input) {\n"
      "  return ::google::protobuf::internal::WireFormat::ParseAndMergePartial(\n"
      "    input, this);\n"
      "}\n",
      "classname", classname_);
    return;
  }

  // Note: If we just switched on the tag rather than the field number, we
  // could avoid the need for the if() to check the wire type at the
  // beginning of each case.  However, this is actually a bit slower in
  // practice as it creates a jump table that is 8x larger and sparser, and
  // meanwhile the ifs are highly predictable.
  printer->Print(
      "(cl:defmethod merge ((self $classname$) buffer start limit)\n"
      "  (cl:declare (cl:type base:octet-vector buffer)\n"
      "              (cl:type base:octet-vector-index start limit))\n"
      "    (cl:do ((index start index))\n"
      "        ((cl:>= index limit) index)\n"
      "      (cl:declare (cl:type base:octet-vector-index index))\n"
      "      (cl:multiple-value-bind (tag new-index)\n"
      "          (varint:parse-uint32-carefully buffer index limit)\n"
      "        (cl:setf index new-index)\n"
      "        (cl:case (cl:ash tag -3)\n");
  printer->Indent();
  printer->Indent();
  printer->Indent();
  printer->Indent();

  scoped_array<const FieldDescriptor*> ordered_fields(
      SortFieldsByNumber(descriptor_));

  for (int i = 0; i < descriptor_->field_count(); i++) {
    const FieldDescriptor* field = ordered_fields[i];

    PrintFieldComment(printer, field);

    string wire_type =
        SimpleItoa(WireFormat::WireTypeForFieldType(field->type()));
    printer->Print(
        "(($number$)\n"
        " (cl:when (/= (cl:logand tag 7) $wiretype$)\n"
        "   (go handle-uninterpreted))\n",
        "number", SimpleItoa(field->number()),
        "wiretype", wire_type);

    if (i > 0 || field->is_repeated()) {
      printer->Print(
          " parse-$name$\n",
          "name", field->name());
    }

    printer->Indent();

    field_generators_.get(field).GenerateMergeFromCodedStream(printer);

#if 0
    // switch() is slow since it can't be predicted well.  Insert some if()s
    // here that attempt to predict the next tag.
    if (field->is_repeated()) {
      // Expect repeats of this field.
      printer->Print(
          "if (input->ExpectTag($tag$)) goto parse_$name$;\n",
          "tag", SimpleItoa(WireFormat::MakeTag(field)),
          "name", field->name());
    }

    if (i + 1 < descriptor_->field_count()) {
      // Expect the next field in order.
      const FieldDescriptor* next_field = ordered_fields[i + 1];
      printer->Print(
          "if (input->ExpectTag($next_tag$)) goto parse_$next_name$;\n",
          "next_tag", SimpleItoa(WireFormat::MakeTag(next_field)),
          "next_name", next_field->name());
    } else {
      // Expect EOF.
      // TODO(kenton):  Expect group end-tag?
      printer->Print("if (input->ExpectAtEnd()) return true;\n");
    }
#endif

    printer->Outdent();
    printer->Print(")\n\n");
  }

  printer->Print(
      "(cl:t\n"
      "handle-uninterpreted\n");
  printer->Indent();

  // Is this an end-group tag?  If so, this must be the end of the message.
  printer->Print(
    "if (::google::protobuf::internal::WireFormat::GetTagWireType(tag) ==\n"
    "    ::google::protobuf::internal::WireFormat::WIRETYPE_END_GROUP) {\n"
    "  return true;\n"
    "}\n");

#if 0
  // Handle extension ranges.
  if (descriptor_->extension_range_count() > 0) {
    printer->Print(
      "if (");
    for (int i = 0; i < descriptor_->extension_range_count(); i++) {
      const Descriptor::ExtensionRange* range =
        descriptor_->extension_range(i);
      if (i > 0) printer->Print(" ||\n    ");

      uint32 start_tag = WireFormatLite::MakeTag(
        range->start, static_cast<WireFormatLite::WireType>(0));
      uint32 end_tag = WireFormatLite::MakeTag(
        range->end, static_cast<WireFormatLite::WireType>(0));

      if (range->end > FieldDescriptor::kMaxNumber) {
        printer->Print(
          "($start$u <= tag)",
          "start", SimpleItoa(start_tag));
      } else {
        printer->Print(
          "($start$u <= tag && tag < $end$u)",
          "start", SimpleItoa(start_tag),
          "end", SimpleItoa(end_tag));
      }
    }
    printer->Print(") {\n"
      "  DO_(_extensions_.ParseField(tag, input, this));\n"
      "  continue;\n"
      "}\n");
  }
#endif

  // We really don't recognize this tag.  Skip it.
  printer->Print(
    "DO_(::google::protobuf::internal::WireFormat::SkipField(\n"
    "      input, tag, mutable_unknown_fields()));\n");

  if (descriptor_->field_count() > 0) {
    printer->Print("break;\n");
    printer->Outdent();
    printer->Print("}\n");    // default:
    printer->Outdent();
    printer->Print("}\n");    // switch
  }

  printer->Outdent();
  printer->Outdent();
  printer->Print(
    "  }\n"                   // while
    "  return true;\n"
    "#undef DO_\n"
    "}\n");
}











void MessageGenerator::GenerateMergeFrom(io::Printer* printer) {
  // Generate the generalized MergeFrom (aka that which takes in the Message
  // base class as a parameter).
  printer->Print(
    "void $classname$::MergeFrom(const ::google::protobuf::Message& from) {\n"
    "  GOOGLE_CHECK_NE(&from, this);\n",
    "classname", classname_);
  printer->Indent();

  // Cast the message to the proper type. If we find that the message is
  // *not* of the proper type, we can still call Merge via the reflection
  // system, as the GOOGLE_CHECK above ensured that we have the same descriptor
  // for each message.
  printer->Print(
    "const $classname$* source =\n"
    "  ::google::protobuf::internal::dynamic_cast_if_available<const $classname$*>(\n"
    "    &from);\n"
    "if (source == NULL) {\n"
    "  ::google::protobuf::internal::ReflectionOps::Merge(from, this);\n"
    "} else {\n"
    "  MergeFrom(*source);\n"
    "}\n",
    "classname", classname_);

  printer->Outdent();
  printer->Print("}\n\n");

  // Generate the class-specific MergeFrom, which avoids the GOOGLE_CHECK
  // and cast.
  printer->Print(
    "void $classname$::MergeFrom(const $classname$& from) {\n"
    "  GOOGLE_CHECK_NE(&from, this);\n",
    "classname", classname_);
  printer->Indent();

  // Merge Repeated fields. These fields do not require a
  // check as we can simply iterate over them.
  for (int i = 0; i < descriptor_->field_count(); ++i) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (field->is_repeated()) {
      field_generators_.get(field).GenerateMergingCode(printer);
    }
  }

  // Merge Optional and Required fields (after a _has_bit check).
  int last_index = -1;

  for (int i = 0; i < descriptor_->field_count(); ++i) {
    const FieldDescriptor* field = descriptor_->field(i);

    if (!field->is_repeated()) {
      map<string, string> vars;
      vars["index"] = SimpleItoa(field->index());

      // See above in GenerateClear for an explanation of this.
      if (i / 8 != last_index / 8 || last_index < 0) {
        if (last_index >= 0) {
          printer->Outdent();
          printer->Print("}\n");
        }
        printer->Print(vars,
          "if (from._has_bits_[$index$ / 32] & (0xffu << ($index$ % 32))) {\n");
        printer->Indent();
      }

      last_index = i;

      printer->Print(vars,
        "if (from._has_bit($index$)) {\n");
      printer->Indent();

      field_generators_.get(field).GenerateMergingCode(printer);

      printer->Outdent();
      printer->Print("}\n");
    }
  }

  if (last_index >= 0) {
    printer->Outdent();
    printer->Print("}\n");
  }

  if (descriptor_->extension_range_count() > 0) {
    printer->Print("_extensions_.MergeFrom(from._extensions_);\n");
  }

  printer->Print(
    "mutable_unknown_fields()->MergeFrom(from.unknown_fields());\n");

  printer->Outdent();
  printer->Print("}\n");
}

}  // namespace lisp
}  // namespace compiler
}  // namespace protobuf
}  // namespace google

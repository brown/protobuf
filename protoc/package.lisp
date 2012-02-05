;;;; Copyright 2012 Google Inc.  All Rights Reserved

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;     * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;     * Neither the name of Google Inc. nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Author: brown@google.com (Robert Brown)

(in-package #:common-lisp-user)

(defpackage #:protoc
  (:documentation "Protocol buffer Common Lisp plugin.")
  (:use #:common-lisp
        #:com.google.base)
  (:import-from #:com.google.protobuf
                ;; Constants
                #:+field-descriptor-proto-label-label-optional+
                #:+field-descriptor-proto-label-label-repeated+
                #:+field-descriptor-proto-label-label-required+
                #:+field-descriptor-proto-type-type-bool+
                #:+field-descriptor-proto-type-type-bytes+
                #:+field-descriptor-proto-type-type-double+
                #:+field-descriptor-proto-type-type-enum+
                #:+field-descriptor-proto-type-type-fixed32+
                #:+field-descriptor-proto-type-type-fixed64+
                #:+field-descriptor-proto-type-type-float+
                #:+field-descriptor-proto-type-type-group+
                #:+field-descriptor-proto-type-type-int32+
                #:+field-descriptor-proto-type-type-int64+
                #:+field-descriptor-proto-type-type-message+
                #:+field-descriptor-proto-type-type-sfixed32+
                #:+field-descriptor-proto-type-type-sfixed64+
                #:+field-descriptor-proto-type-type-sint32+
                #:+field-descriptor-proto-type-type-sint64+
                #:+field-descriptor-proto-type-type-string+
                #:+field-descriptor-proto-type-type-uint32+
                #:+field-descriptor-proto-type-type-uint64+
                #:+field-options-ctype-cord+
                #:+field-options-ctype-string+
                #:+field-options-ctype-string-piece+
                #:+file-options-optimize-mode-code-size+
                #:+file-options-optimize-mode-lite-runtime+
                #:+file-options-optimize-mode-speed+
                #:+maximum-field-descriptor-proto-label+
                #:+maximum-field-descriptor-proto-type+
                #:+maximum-field-options-ctype+
                #:+maximum-file-options-optimize-mode+
                #:+minimum-field-descriptor-proto-label+
                #:+minimum-field-descriptor-proto-type+
                #:+minimum-field-options-ctype+
                #:+minimum-file-options-optimize-mode+
                ;; Classes
                #:descriptor-proto
                #:descriptor-proto-extension-range
                #:enum-descriptor-proto
                #:enum-options
                #:enum-value-descriptor-proto
                #:enum-value-options
                #:field-descriptor-proto
                #:field-descriptor-proto-label
                #:field-descriptor-proto-type
                #:field-options
                #:field-options-ctype
                #:file-descriptor-proto
                #:file-descriptor-set
                #:file-options
                #:file-options-optimize-mode
                #:message-options
                #:method-descriptor-proto
                #:method-options
                #:service-descriptor-proto
                #:service-options
                #:source-code-info-location
                #:uninterpreted-option-name-part
                ;; Functions
                #:aggregate-value
                #:cc-generic-services
                #:clear-aggregate-value
                #:clear-cc-generic-services
                #:clear-ctype
                #:clear-default-value
                #:clear-dependency
                #:clear-deprecated
                #:clear-double-value
                #:clear-end
                #:clear-enum-type
                #:clear-experimental-map-key
                #:clear-extendee
                #:clear-extension
                #:clear-extension-range
                #:clear-field
                #:clear-file
                #:clear-identifier-value
                #:clear-input-type
                #:clear-is-extension
                #:clear-java-generate-equals-and-hash
                #:clear-java-generic-services
                #:clear-java-multiple-files
                #:clear-java-outer-classname
                #:clear-java-package
                #:clear-label
                #:clear-location
                #:clear-message-set-wire-format
                #:clear-message-type
                #:clear-method
                #:clear-name
                #:clear-name-part
                #:clear-negative-int-value
                #:clear-nested-type
                #:clear-no-standard-descriptor-accessor
                #:clear-number
                #:clear-optimize-for
                #:clear-options
                #:clear-output-type
                #:clear-package
                #:clear-packed
                #:clear-path
                #:clear-positive-int-value
                #:clear-py-generic-services
                #:clear-service
                #:clear-source-code-info
                #:clear-span
                #:clear-start
                #:clear-string-value
                #:clear-type
                #:clear-type-name
                #:clear-uninterpreted-option
                #:clear-value
                #:ctype
                #:default-value
                #:dependency
                #:deprecated
                #:double-value
                #:end
                #:enum-type
                #:experimental-map-key
                #:extendee
                #:extension
                #:extension-range
                #:field
                #:file
                #:has-aggregate-value
                #:has-cc-generic-services
                #:has-ctype
                #:has-default-value
                #:has-deprecated
                #:has-double-value
                #:has-end
                #:has-experimental-map-key
                #:has-extendee
                #:has-identifier-value
                #:has-input-type
                #:has-is-extension
                #:has-java-generate-equals-and-hash
                #:has-java-generic-services
                #:has-java-multiple-files
                #:has-java-outer-classname
                #:has-java-package
                #:has-label
                #:has-message-set-wire-format
                #:has-name
                #:has-name-part
                #:has-negative-int-value
                #:has-no-standard-descriptor-accessor
                #:has-number
                #:has-optimize-for
                #:has-options
                #:has-output-type
                #:has-package
                #:has-packed
                #:has-positive-int-value
                #:has-py-generic-services
                #:has-source-code-info
                #:has-start
                #:has-string-value
                #:has-type
                #:has-type-name
                #:identifier-value
                #:input-type
                #:is-extension
                #:java-generate-equals-and-hash
                #:java-generic-services
                #:java-multiple-files
                #:java-outer-classname
                #:java-package
                #:label
                #:location
                #:message-set-wire-format
                #:message-type
                #:name
                #:name-part
                #:negative-int-value
                #:nested-type
                #:no-standard-descriptor-accessor
                #:optimize-for
                #:options
                #:output-type
                #:packed
                #:path
                #:positive-int-value
                #:py-generic-services
                #:service
                #:source-code-info
                #:span
                #:start
                #:string-value
                #:type-name
                #:uninterpreted-option
                #:value)
  ;; Descriptor symbols that conflict with standard Common Lisp symbols.
  (:shadowing-import-from #:com.google.protobuf
                          #:method
                          #:number
                          #:package
                          #:type)
  (:export #:main))

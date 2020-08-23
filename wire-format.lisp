;;;; Copyright 2010 Google Inc.  All Rights Reserved

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

;;;; Author: Robert Brown <robert.brown@gmail.com>

;;;; Wire format used when reading and writing protobuf data.

(in-package #:wire-format)

(defconst +varint+ 0 "Wire type used for variable length integers, booleans, and enums.")
(defconst +fixed64+ 1 "Wire type used for 8-byte integers or double precision floats.")
(defconst +length-delimited+ 2
  "Wire type used for length-delimited values, such as strings, bytes, embedded
messages, and packed repeated fields.")
(defconst +start-group+ 3 "Wire type marking the start of a group.  Deprecated.")
(defconst +end-group+ 4 "Wire type marking the end of a group.  Deprecated.")
(defconst +fixed32+ 5 "Wire type used for 4-byte integers or single precision floats.")

(deftype wire-type ()
  "Integer representing how a protobuf field value is serialized."
  `(member ,+varint+ ,+fixed32+ ,+length-delimited+ ,+start-group+ ,+end-group+ ,+fixed64+))

(deftype field-number ()
  "Protocol buffer field number."
  ;; A field number is a 29-bit positive integer, but zero is illegal.  Field numbers from 19000
  ;; through 19999 are reserved for internal use by protocol buffer implementations.
  `(integer 1 ,(ldb (byte 29 0) -1)))

(define-condition protocol-error (error)
  ()
  (:documentation "Superclass of all PROTOCOL-BUFFER conditions."))

(define-condition encoding-error (protocol-error)
  ()
  (:documentation "Superclass of conditions signalled while encoding values."))

(define-condition buffer-overflow (encoding-error)
  ()
  (:documentation "Buffer space exhausted while encoding a value."))

(define-condition parsing-error (protocol-error)
  ()
  (:documentation "Superclass of conditions signalled while decoding values."))

(define-condition data-exhausted (parsing-error)
  ()
  (:documentation "Decoding a value requires more data than is available."))

(define-condition value-out-of-range (parsing-error)
  ()
  (:documentation "Value decoded is outside the range of the return type."))

(define-condition alignment (parsing-error)
  ()
  (:documentation "Bad data encountered while skipping a field."))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values field-number wire-type vector-index &optional))
                parse-tag)
         #+opt (inline parse-tag))

(defun parse-tag (buffer index limit)
  "Parses the varint protobuf tag at position INDEX of BUFFER, being careful to
only read positions below LIMIT.  When successful, returns three integers: the
field number, the encoding wire type, and the position in BUFFER where the
field value is stored.

PARSE-TAG signals DATA-EXHAUSTED, when parsing the tag requires reading beyond
LIMIT, and signals VALUE-OUT-OF-RANGE, when the field number is zero or the
encoded tag is too large."
  (multiple-value-bind (tag new-index)
      (varint:parse-uint32-carefully buffer index limit)
    (let ((field-number (ldb (byte 29 3) tag))
          (wire-type (ldb (byte 3 0) tag)))
      (when (zerop field-number) (error 'value-out-of-range))
      (values field-number wire-type new-index))))

(declaim (ftype (function (field-number wire-type octet-vector vector-index vector-index)
                          (values vector-index &optional))
                skip-field))

(defun skip-field (field-number wire-type buffer index limit)
  (declare (type field-number field-number)
           (type wire-type wire-type)
           (type octet-vector buffer)
           (type vector-index index limit))
  (case wire-type
    (#.+varint+
     (varint:skip64-carefully buffer index limit))
    (#.+fixed64+
     (let ((new-index (+ index 8)))
       (declare (type vector-index new-index))
       (when (> new-index limit) (error 'data-exhausted))
       new-index))
    (#.+length-delimited+
     (multiple-value-bind (size new-index)
         (varint:parse-uint32-carefully buffer index limit)
       (declare (type uint32 size)
                (type vector-index new-index))
       (when (> (+ new-index size) limit) (error 'data-exhausted))
       (the vector-index (+ new-index size))))
    (#.+start-group+
     (loop
       (multiple-value-bind (element-field-number element-wire-type new-index)
           (parse-tag buffer index limit)
         (cond ((/= element-wire-type +end-group+)
                (setf index
                      (skip-field element-field-number element-wire-type buffer new-index limit)))
               ((= element-field-number field-number) (return new-index))
               (t (error 'alignment))))))
    (#.+fixed32+
     (let ((new-index (+ index 4)))
       (declare (type vector-index new-index))
       (when (> new-index index) (error 'data-exhausted))
       new-index))
    (t
     (error 'alignment))))

(declaim (ftype (function (octet-vector vector-index vector-index boolean)
                          (values vector-index &optional))
                write-boolean-carefully)
         #+opt (inline write-boolean-carefully))

(defun write-boolean-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type boolean value))
  (let ((new-index (1+ index)))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (aref buffer index) (if value 1 0))
    new-index))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values boolean vector-index &optional))
                read-boolean-carefully)
         #+opt (inline read-boolean-carefully))

(defun read-boolean-carefully (buffer index limit)
  (unless (< index limit) (error 'data-exhausted))
  (let ((bool (aref buffer index)))
    (if (or (= bool 0) (= bool 1))
        (values (if (zerop bool) nil t) (1+ index))
        (multiple-value-bind (bool new-index)
            (varint:parse-uint64-carefully buffer index limit)
          (values (if (zerop bool) nil t) new-index)))))

(declaim (ftype (function (octet-vector vector-index vector-index int32)
                          (values vector-index &optional))
                write-int32-carefully)
         #+opt (inline write-int32-carefully))

(defun write-int32-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type int32 value))
  (let ((new-index (+ index 4)))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (nibbles:sb32ref/le buffer index) value)
    new-index))

(declaim (ftype (function (octet-vector vector-index vector-index uint32)
                          (values vector-index &optional))
                write-uint32-carefully)
         #+opt (inline write-uint32-carefully))

(defun write-uint32-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type uint32 value))
  (let ((new-index (+ index 4)))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (nibbles:ub32ref/le buffer index) value)
    new-index))

(declaim (ftype (function (octet-vector vector-index vector-index int64)
                          (values vector-index &optional))
                write-int64-carefully)
         #+opt (inline write-int64-carefully))

(defun write-int64-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type int64 value))
  (let ((new-index (+ index 8)))
    (declare (type vector-index new-index))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (nibbles:sb64ref/le buffer index) value)
    new-index))

(declaim (ftype (function (octet-vector vector-index vector-index uint64)
                          (values vector-index &optional))
                write-uint64-carefully)
         #+opt (inline write-uint64-carefully))

(defun write-uint64-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type uint64 value))
  (let ((new-index (+ index 8)))
    (declare (type vector-index new-index))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (nibbles:ub64ref/le buffer index) value)
    new-index))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint32 vector-index &optional))
                read-uint32-carefully)
         #+opt (inline read-uint32-carefully))

(defun read-uint32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (let ((new-index (+ index 4)))
    (if (> new-index limit)
        (error 'data-exhausted)
        (values (nibbles:ub32ref/le buffer index) new-index))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values int32 vector-index &optional))
                read-int32-carefully)
         #+opt (inline read-int32-carefully))

(defun read-int32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (let ((new-index (+ index 4)))
    (if (> new-index limit)
        (error 'data-exhausted)
        (values (nibbles:sb32ref/le buffer index) new-index))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint64 vector-index &optional))
                read-uint64-carefully)
         #+opt (inline read-uint64-carefully))

(defun read-uint64-carefully (buffer index limit)
  (declare (type vector-index index limit))
  (let ((new-index (+ index 8)))
    (declare (type vector-index new-index))
    (if (> new-index limit)
        (error 'data-exhausted)
        (values (nibbles:ub64ref/le buffer index) new-index))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values int64 vector-index &optional))
                read-int64-carefully)
         #+opt (inline read-int64-carefully))

(defun read-int64-carefully (buffer index limit)
  (declare (type vector-index index limit))
  (let ((new-index (+ index 8)))
    (declare (type vector-index new-index))
    (if (> new-index limit)
        (error 'data-exhausted)
        (values (nibbles:sb64ref/le buffer index) new-index))))

(declaim (ftype (function (octet-vector
                           vector-index
                           vector-index
                           single-float)
                          (values vector-index &optional))
                write-single-float-carefully)
         #+opt (inline write-single-float-carefully))

(defun write-single-float-carefully (buffer index limit float)
  "Write the little-endian IEEE binary representation of double precision FLOAT
to BUFFER starting at INDEX.  Return the index value of the first octet
following FLOAT.  If encoding FLOAT requires space in BUFFER past LIMIT, then
signal BUFFER-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type single-float float))
  (let ((new-index (+ index 4)))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (nibbles:ieee-single-ref/le buffer index) float)
    new-index))

(declaim (ftype (function (octet-vector
                           vector-index
                           vector-index
                           double-float)
                          (values vector-index &optional))
                write-double-float-carefully)
         #+opt (inline write-double-float-carefully))

(defun write-double-float-carefully (buffer index limit float)
  "Write the little-endian IEEE binary representation of single precision FLOAT
to BUFFER starting at INDEX.  Return the index value of the first octet
following FLOAT.  If encoding FLOAT requires space in BUFFER past LIMIT, then
signal BUFFER-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type double-float float))
  (let ((new-index (+ index 8)))
    (declare (type vector-index new-index))
    (when (> new-index limit) (error 'buffer-overflow))
    (setf (nibbles:ieee-double-ref/le buffer index) float)
    new-index))

(declaim (ftype (function (octet-vector
                           vector-index
                           vector-index)
                          (values single-float vector-index &optional))
                read-single-float-carefully)
         #+opt (inline read-single-float-carefully))

(defun read-single-float-carefully (buffer index limit)
  "Read a SINGLE-FLOAT from BUFFER starting at INDEX.  The float is stored in
BUFFER as a 4-octet little-endian IEEE single precision value.  Both the float
and the index of the first octet following it are returned.  If reading the
float would require octets beyond LIMIT, then signal DATA-EXHAUSTED."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (let ((new-index (+ index 4)))
    (when (> new-index limit) (error 'data-exhausted))
    (values (nibbles:ieee-single-ref/le buffer index) new-index)))

(declaim (ftype (function (octet-vector
                           vector-index
                           vector-index)
                          (values double-float vector-index &optional))
                read-double-float-carefully)
         #+opt (inline read-double-float-carefully))

(defun read-double-float-carefully (buffer index limit)
  "Read a DOUBLE-FLOAT from BUFFER starting at INDEX.  The float is stored in
BUFFER as an 8-octet little-endian IEEE double precision value.  Both the float
and the index of the first octet following it are returned.  If reading the
float would require octets beyond LIMIT, then signal DATA-EXHAUSTED."
  (declare (type octet-vector buffer)
           (type vector-index index))
  (let ((new-index (+ index 8)))
    (declare (type vector-index new-index))
    (when (> new-index limit) (error 'data-exhausted))
    (values (nibbles:ieee-double-ref/le buffer index) new-index)))

(declaim (ftype (function (octet-vector vector-index vector-index octet-vector)
                          (values vector-index &optional))
                write-octets-carefully)
         #+opt (inline write-octets-carefully))

(defun write-octets-carefully (buffer index limit octets)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type octet-vector octets))
  (let ((size (length octets)))
    (setf index (varint:encode-uint32-carefully buffer index limit size))
    (let ((new-index (+ index size)))
      (declare (type vector-index new-index))
      (when (> new-index limit) (error 'buffer-overflow))
      (replace buffer octets :start1 index)
      new-index)))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values octet-vector vector-index &optional))
                read-octets-carefully)
         #+opt (inline read-octets-carefully))

(defun read-octets-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (multiple-value-bind (length start-index)
      (varint:parse-uint32-carefully buffer index limit)
    (let ((end-index (+ start-index length)))
      (declare (type vector-index end-index))
      (when (> end-index limit) (error 'data-exhausted))
      (values (subseq buffer start-index end-index) end-index))))

(declaim (ftype (function (int32) (values uint32 &optional)) zig-zag-encode32)
         (inline zig-zag-encode32))

(defun zig-zag-encode32 (v)
  (declare (type int32 v))
  (logand (logxor (ash v 1) (ash v -31)) #xffffffff))

(declaim (ftype (function (uint32) (values int32 &optional)) zig-zag-decode32)
         (inline zig-zag-decode32))

(defun zig-zag-decode32 (v)
  (declare (type uint32 v))
  (logxor (ash v -1) (- (logand v 1))))

(declaim (ftype (function (int64) (values uint64 &optional)) zig-zag-encode64)
         (inline zig-zag-encode64))

(defun zig-zag-encode64 (v)
  (declare (type int64 v))
  (logand (logxor (ash v 1) (ash v -63)) #xffffffffffffffff))

(declaim (ftype (function (uint64) (values int64 &optional)) zig-zag-decode64)
         (inline zig-zag-decode64))

(defun zig-zag-decode64 (v)
  (declare (type uint64 v))
  (logxor (ash v -1) (- (logand v 1))))

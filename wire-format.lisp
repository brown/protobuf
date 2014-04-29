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

;;;; Author: brown@google.com (Robert Brown)

;;;; Wire format used when reading and writing protobuf data.

(in-package #:wire-format)
(declaim #.*optimize-default*)

(deftype wire-type ()
  "Integer determining how a value is serialized."
  '(integer 0 5))

(defconst +varint+ 0 "Wire type used for variable length integers.")
(defconst +fixed64+ 1 "Wire type used for 8-byte integers or double precision floats.")
(defconst +length-delimited+ 2
  "Wire type used for length delimited values, such as character strings.")
(defconst +start-group+ 3 "Wire type marking the start of a group.")
(defconst +end-group+ 4 "Wire type marking the end of a group.")
(defconst +fixed32+ 5 "Wire type used for 4-byte integers or single precision floats.")

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

(declaim (ftype (function (octet-vector vector-index vector-index fixnum)
                          (values vector-index &optional))
                skip-field))

(defun skip-field (buffer index limit start-tag)
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type fixnum start-tag))     ; TODO(brown): Use better type here and above.
  (case (ldb (byte 3 0) start-tag)
    (#.+varint+ (varint:skip64-carefully buffer index limit))
    (#.+fixed64+
     (let ((new-index (+ index 8)))
       (declare (type vector-index new-index))
       (when (> new-index index) (error 'data-exhausted))
       new-index))
    (#.+length-delimited+
     (multiple-value-bind (size new-index)
         (varint:parse-uint32-carefully buffer index limit)
       (the vector-index (+ new-index size))))
    (#.+start-group+
     (loop (multiple-value-bind (tag new-index)
               (varint:parse-uint32-carefully buffer index limit)
             (cond ((/= (ldb (byte 3 0) tag) +end-group+)
                    (setf index (skip-field buffer new-index limit tag)))
                   ((= (- start-tag +start-group+) (- tag +end-group+))
                    (return new-index))
                   (t (error 'alignment))))))
    (#.+fixed32+
     (let ((new-index (+ index 4)))
       (declare (type vector-index new-index))
       (when (> new-index index) (error 'data-exhausted))
       new-index))
    (t (error 'alignment))))

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
  (let ((new-index (1+ index)))
    (when (> new-index limit) (error 'data-exhausted))
    (let ((bool (aref buffer index)))
      (unless (or (= bool 0) (= bool 1)) (error 'value-out-of-range))
      (values (if (zerop bool) nil t) new-index))))

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
signal ENCODE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type single-float float))
  (when (> (+ index 4) limit) (error 'buffer-overflow))
  (let ((bits #-(or abcl allegro cmu lispworks sbcl) (portable-float:single-float-bits float)
              #+abcl (system:single-float-bits float)
              #+allegro (multiple-value-bind (high low)
                            (excl:single-float-to-shorts float)
                          (declare (type (unsigned-byte 16) high low))
                          (logior (ash high 16) low))
              #+cmu (kernel:single-float-bits float)
              #+lispworks (lispworks-float:single-float-bits float)
              #+sbcl (sb-kernel:single-float-bits float)))
    (declare (type #-allegro int32 #+allegro uint32 bits))
    #-allegro (setf (nibbles:sb32ref/le buffer index) bits)
    #+allegro (setf (nibbles:ub32ref/le buffer index) bits)
    (incf index 4)))

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
signal ENCODE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type double-float float))
  (when (> (+ index 8) limit) (error 'buffer-overflow))
  (let ((low 0)
        (high 0))
    (declare (type uint32 low)
             (type #-allegro int32 #+allegro uint32 high))
    #-(or abcl allegro cmu lispworks sbcl)
    (let ((bits (portable-float:double-float-bits float)))
      (setf low (logand #xffffffff bits))
      (setf high (ash bits -32)))
    #+abcl
    (progn (setf low (system:double-float-low-bits float))
           (setf high (system:double-float-high-bits float)))
    #+allegro
    (multiple-value-bind (us3 us2 us1 us0)
        (excl:double-float-to-shorts float)
      (declare (type (unsigned-byte 16) us3 us2 us1 us0))
      (setf low (logior (ash us1 16) us0))
      (setf high (logior (ash us3 16) us2)))
    #+cmu
    (progn (setf low (kernel:double-float-low-bits float))
           (setf high (kernel:double-float-high-bits float)))
    #+lispworks
    (let ((bits (lispworks-float:double-float-bits float)))
      (setf low (logand #xffffffff bits))
      (setf high (ash bits -32)))
    #+sbcl
    (progn (setf low (sb-kernel:double-float-low-bits float))
           (setf high (sb-kernel:double-float-high-bits float)))
    (setf (nibbles:ub32ref/le buffer index) low)
    (incf index 4)
    #-allegro (setf (nibbles:sb32ref/le buffer index) high)
    #+allegro (setf (nibbles:ub32ref/le buffer index) high)
    (incf index 4)))

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
float would require octets beyond LIMIT, then signal PARSE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (when (> (+ index 4) limit) (error 'data-exhausted))
  (let ((bits (nibbles:sb32ref/le buffer index)))
    (incf index 4)
    #-(or abcl allegro cmu lispworks sbcl)
    (values (portable-float:make-single-float bits) index)
    #+abcl
    (values (system:make-single-float bits) index)
    #+allegro
    (values (excl:shorts-to-single-float (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
            index)
    #+cmu
    (values (kernel:make-single-float bits) index)
    #+lispworks
    (values (lispworks-float:make-single-float bits) index)
    #+sbcl
    (values (sb-kernel:make-single-float bits) index)))

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
float would require octets beyond LIMIT, then signal PARSE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index))
  (when (> (+ index 8) limit) (error 'data-exhausted))
  (let ((low (nibbles:ub32ref/le buffer index)))
    (incf index 4)
    (let ((high (nibbles:sb32ref/le buffer index)))
      (incf index 4)
      #-(or abcl allegro cmu lispworks sbcl)
      (values (portable-float:make-double-float high low) index)
      #+abcl
      (values (system:make-double-float (logior (ash high 32) low)) index)
      #+allegro
      (values (excl:shorts-to-double-float (ldb (byte 16 16) high)
                                           (ldb (byte 16 0) high)
                                           (ldb (byte 16 16) low)
                                           (ldb (byte 16 0) low))
              index)
      #+cmu
      (values (kernel:make-double-float high low) index)
      #+lispworks
      (values (lispworks-float:make-double-float high low) index)
      #+sbcl
      (values (sb-kernel:make-double-float high low) index))))

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

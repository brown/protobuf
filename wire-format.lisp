
;;;;    wire-format.lisp


;; Copyright 2010, Google Inc. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;     * Neither the name of Google Inc. nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package #:wire-format)

(declaim #.optimize:+default+)


;; Tags used to delimit values in protocol buffers.

(deftype protocol-tag () '(integer 0 5))

(defconst +numeric+     0   "tag for varint integers")
(defconst +double+      1   "tag for 8-byte double precision floats")
(defconst +string+      2   "tag for character strings")
(defconst +start-group+ 3   "tag marks start of embedded protocol buffer")
(defconst +end-group+   4   "tag marks end of embedded protocol buffer")
(defconst +float+       5   "tag for 4-byte single precision floats")


(define-condition protocol-error (error)
  ()
  (:documentation "Superclass of all PROTOCOL conditions."))


(define-condition encoding-error (protocol-error)
  ()
  (:documentation "Superclass of all PROTOCOL conditions signalled while
encoding values into a buffer."))

(define-condition buffer-overflow (encoding-error)
  ()
  (:documentation "Buffer space exhausted while encoding a value."))


(define-condition parsing-error (protocol-error)
  ()
  (:documentation "Superclass of all PROTCOL conditions signalled while
decoding values from a buffer."))

(define-condition data-exhausted (parsing-error)
  ()
  (:documentation "Decoding a value requires more data than is available."))

(define-condition value-out-of-range (parsing-error)
  ()
  (:documentation "Value decoded is outside the range of the return type."))

(define-condition alignment (parsing-error)
  ()
  (:documentation "Data buffer does not contain the type of value we have
been asked to skip over of parse backwards."))


(declaim (ftype (function (octet-vector octet-vector-index fixnum)
                          octet-vector-index)
                skip-element))

(defun skip-element (buffer index start-code)
  (declare (type octet-vector-index index))
  (let ((tag (ldb (byte 3 0) start-code)))
    (cond ((= tag +numeric+) (varint:skip64 buffer index))
          ((= tag +double+) (+ index 8))
          ((= tag +string+)
           (multiple-value-bind (size new-index)
               (varint:parse-uint32 buffer index)
             (+ new-index size)))
          ((= tag +start-group+)
           (loop (multiple-value-bind (code new-index)
                     (varint:parse-uint32 buffer index)
                   (if (/= (ldb (byte 3 0) code) +end-group+)
                       (setf index (skip-element buffer new-index code))
                       (progn (when (/= (- start-code +start-group+)
                                        (- code +end-group+))
                                (error 'alignment))
                              (return-from skip-element new-index))))))
          ((= tag +float+) (+ index 4))
          (t (error 'alignment)))))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           boolean)
                          octet-vector-index)
                write-boolean-carefully)
         #+opt (inline write-boolean-carefully))

(defun write-boolean-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type boolean value))
  (when (>= index limit)
    (error 'buffer-overflow))
  (setf (aref buffer index) (if value 1 0))
  (incf index)
  index)

(declaim (ftype (function (octet-vector octet-vector-index octet-vector-index)
                          (values boolean octet-vector-index))
                read-boolean-carefully)
         #+opt (inline read-boolean-carefully))

(defun read-boolean-carefully (buffer index limit)
  (when (>= index limit)
    (error 'data-exhausted))
  (let ((bool (aref buffer index)))
    (unless (or (= bool 0) (= bool 1))
      (error 'value-out-of-range))
    (values (if (zerop bool) nil t) (1+ index))))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           int32)
                          octet-vector-index)
                write-int32-carefully)
         #+opt (inline write-int32-carefully))

(defun write-int32-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type int32 value))
  (when (> (+ index 4) limit)
    (error 'buffer-overflow))
  (setf (aref buffer index) (ldb (byte 8 0) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 8) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 16) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 24) value))
  (incf index)
  index)

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           uint32)
                          octet-vector-index)
                write-uint32-carefully)
         #+opt (inline write-uint32-carefully))

(defun write-uint32-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type uint32 value))
  (when (> (+ index 4) limit)
    (error 'buffer-overflow))
  (setf (aref buffer index) (ldb (byte 8 0) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 8) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 16) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 24) value))
  (incf index)
  index)

(declaim (ftype (function (octet-vector octet-vector-index uint32) (values))
                write-uint32)
         #+opt (inline write-uint32))

(defun write-uint32 (buffer index value)
  (declare (type octet-vector buffer)
           (type octet-vector-index index)
           (type uint32 value))
  (setf (aref buffer index) (ldb (byte 8 0) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 8) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 16) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 24) value)))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           int64)
                          octet-vector-index)
                write-int64-carefully)
         #+opt (inline write-int64-carefully))

(defun write-int64-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type int64 value))
  (when (> (+ index 8) limit)
    (error 'buffer-overflow))
  (setf (aref buffer index) (ldb (byte 8 0) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 8) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 16) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 24) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 32) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 40) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 48) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 56) value))
  (incf index)
  index)

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           uint64)
                          octet-vector-index)
                write-uint64-carefully)
         #+opt (inline write-uint64-carefully))

(defun write-uint64-carefully (buffer index limit value)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type uint64 value))
  (when (> (+ index 8) limit)
    (error 'buffer-overflow))
  (setf (aref buffer index) (ldb (byte 8 0) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 8) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 16) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 24) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 32) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 40) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 48) value))
  (incf index)
  (setf (aref buffer index) (ldb (byte 8 56) value))
  (incf index)
  index)

(declaim (ftype (function (octet-vector octet-vector-index octet-vector-index)
                          (values uint32 octet-vector-index))
                read-uint32-carefully)
         #+opt (inline read-uint32-carefully))

(defun read-uint32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit))
  (when (> (+ index 4) limit)
    (error 'data-exhausted))
  (let ((result (aref buffer index)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 8)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 16)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 24)))
    (incf index)
    (values result index)))

(declaim (ftype (function (octet-vector octet-vector-index octet-vector-index)
                          (values int32 octet-vector-index))
                read-int32-carefully)
         #+opt (inline read-int32-carefully))

(defun read-int32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit))
  (multiple-value-bind (result new-index)
      (read-uint32-carefully buffer index limit)
    (when (= (ldb (byte 1 31) result) 1)    ; sign bit set, so negative value
      (decf result (ash 1 32)))
    (values result new-index)))

(declaim (ftype (function (octet-vector octet-vector-index) uint32)
                read-uint32)
         #+opt (inline read-uint32))

(defun read-uint32 (buffer index)
  (declare (type octet-vector buffer)
           (type octet-vector-index index))
  (let ((result (aref buffer index)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 8)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 16)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 24)))
    result))

(declaim (ftype (function (octet-vector octet-vector-index octet-vector-index)
                          (values uint64 octet-vector-index))
                read-uint64-carefully)
         #+opt (inline read-uint64-carefully))

(defun read-uint64-carefully (buffer index limit)
  (declare (type octet-vector-index index limit))
  (when (> (+ index 8) limit)
    (error 'data-exhausted))
  (let ((result (aref buffer index)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 8)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 16)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 24)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 32)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 40)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 48)))
    (incf index)
    (setf result (logior result (ash (aref buffer index) 56)))
    (incf index)
    (values result index)))

(declaim (ftype (function (octet-vector octet-vector-index octet-vector-index)
                          (values int64 octet-vector-index))
                read-int64-carefully)
         #+opt (inline read-int64-carefully))

(defun read-int64-carefully (buffer index limit)
  (declare (type octet-vector-index index limit))
  (multiple-value-bind (result new-index)
      (read-uint64-carefully buffer index limit)
    (when (= (ldb (byte 1 63) result) 1)    ; sign bit set, so negative value
      (decf result (ash 1 64)))
    (values result new-index)))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           single-float)
                          octet-vector-index)
                write-single-float-carefully)
         #+opt (inline write-single-float-carefully))

(defun write-single-float-carefully (buffer index limit float)
  "Write the little-endian IEEE binary representation of double precision
FLOAT to BUFFER starting at INDEX.  Return the index value of the first
octet following FLOAT.  If encoding FLOAT requires space in BUFFER past
LIMIT, then signal ENCODE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type single-float float))
  (when (> (+ index 4) limit)
    (error 'buffer-overflow))
  (let ((bits #-(or abcl allegro cmu sbcl)
              (portable-float:single-float-bits float)
              #+abcl (system:single-float-bits float)
              #+allegro
              (multiple-value-bind (high low)
                  (excl:single-float-to-shorts float)
                (declare (type (unsigned-byte 16) high low))
                (logior (ash high 16) low))
              #+cmu (kernel:single-float-bits float)
              #+sbcl (sb-kernel:single-float-bits float)))
    (declare (type #-allegro int32 #+allegro uint32 bits))
    (setf (aref buffer index) (ldb (byte 8 0) bits))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 8) bits))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 16) bits))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 24) bits))
    (incf index)
    index))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           double-float)
                          octet-vector-index)
                write-double-float-carefully)
         #+opt (inline write-double-float-carefully))

(defun write-double-float-carefully (buffer index limit float)
  "Write the little-endian IEEE binary representation of single precision
FLOAT to BUFFER starting at INDEX.  Return the index value of the first
octet following FLOAT.  If encoding FLOAT requires space in BUFFER past
LIMIT, then signal ENCODE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type double-float float))
  (when (> (+ index 8) limit)
    (error 'buffer-overflow))
  (let ((low 0)
        (high 0))
    (declare (type uint32 low)
             (type #-allegro int32 #+allegro uint32 high))
    #-(or abcl allegro cmu sbcl)
    (let ((bits (portable-float:double-float-bits float)))
      (setf low (logand #xffffffff bits))
      (setf high (portable-float:mask-and-sign-extend (ash bits -32) 32)))
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
    #+sbcl
    (progn (setf low (sb-kernel:double-float-low-bits float))
           (setf high (sb-kernel:double-float-high-bits float)))
    (setf (aref buffer index) (ldb (byte 8 0) low))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 8) low))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 16) low))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 24) low))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 0) high))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 8) high))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 16) high))
    (incf index)
    (setf (aref buffer index) (ldb (byte 8 24) high))
    (incf index))
  index)

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index)
                          (values single-float octet-vector-index))
                read-single-float-carefully)
         #+opt (inline read-single-float-carefully))

(defun read-single-float-carefully (buffer index limit)
  "Read a SINGLE-FLOAT from BUFFER starting at INDEX.  The float is stored
in BUFFER as a 4-octet little-endian IEEE single precision value.  Both the
float and the index of the first octet following it are returned.  If
reading the float would require octets beyond LIMIT, then signal
PARSE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit))
  (when (> (+ index 4) limit)
    (error 'data-exhausted))
  (let ((bits (aref buffer index)))
    (incf index)
    (setf bits (logior bits (ash (aref buffer index) 8)))
    (incf index)
    (setf bits (logior bits (ash (aref buffer index) 16)))
    (incf index)
    (setf bits (logior bits (ash (aref buffer index) 24)))
    (incf index)
    ;; BITS must have the correct sign.
    (when (= (ldb (byte 1 31) bits) 1)    ; sign bit set, so negative value
      (decf bits (ash 1 32)))
    #-(or abcl allegro cmu sbcl)
    (values (portable-float:make-single-float bits) index)
    #+abcl
    (values (system:make-single-float bits) index)
    #+allegro
    (values (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                                         (ldb (byte 16 0) bits))
            index)
    #+cmu
    (values (kernel:make-single-float bits) index)
    #+sbcl
    (values (sb-kernel:make-single-float bits) index)))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index)
                          (values double-float octet-vector-index))
                read-double-float-carefully)
         #+opt (inline read-double-float-carefully))

(defun read-double-float-carefully (buffer index limit)
  "Read a DOUBLE-FLOAT from BUFFER starting at INDEX.  The float is stored
in BUFFER as an 8-octet little-endian IEEE double precision value.  Both the
float and the index of the first octet following it are returned.  If
reading the float would require octets beyond LIMIT, then signal
PARSE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type octet-vector-index index))
  (when (> (+ index 8) limit)
    (error 'data-exhausted))
  (let ((low (aref buffer index)))
    (incf index)
    (setf low (logior low (ash (aref buffer index) 8)))
    (incf index)
    (setf low (logior low (ash (aref buffer index) 16)))
    (incf index)
    (setf low (logior low (ash (aref buffer index) 24)))
    (incf index)
    (let ((high (aref buffer index)))
      (incf index)
      (setf high (logior high (ash (aref buffer index) 8)))
      (incf index)
      (setf high (logior high (ash (aref buffer index) 16)))
      (incf index)
      (setf high (logior high (ash (aref buffer index) 24)))
      (incf index)
      ;; High bits are signed, but low bits are unsigned.
      (when (= (ldb (byte 1 31) high) 1)    ; sign bit set, so negative value
        (decf high (ash 1 32)))
      #-(or abcl allegro cmu sbcl)
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
      #+sbcl
      (values (sb-kernel:make-double-float high low) index))))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index
                           octet-vector)
                          octet-vector-index)
                write-octets-carefully)
         #+opt (inline write-octets-carefully))

(defun write-octets-carefully (buffer index limit octets)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit)
           (type octet-vector octets))
  (let ((size (length octets)))
    (setf index (varint:encode-uint32-carefully buffer index limit size))
    (when (> (+ index size) limit)
      (error 'buffer-overflow))
    (replace buffer octets :start1 index)
    (+ index size)))

(declaim (ftype (function (octet-vector
                           octet-vector-index
                           octet-vector-index)
                          (values octet-vector octet-vector-index))
                read-octets-carefully)
         #+opt (inline read-octets-carefully))

(defun read-octets-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type octet-vector-index index limit))
  (multiple-value-bind (length start-index)
      (varint:parse-uint32-carefully buffer index limit)
    (let ((end-index (+ start-index length)))
      (declare (type octet-vector-index end-index))
      (when (> end-index limit)
        (error 'data-exhausted))
      (values (subseq buffer start-index end-index) end-index))))

(declaim (ftype (function (int32) uint32) zig-zag-encode32)
         (inline zig-zag-encode32))

(defun zig-zag-encode32 (v)
  (logxor (ash v 1) (ash v -31)))

(declaim (ftype (function (uint32) int32) zig-zag-decode32)
         (inline zig-zag-decode32))

(defun zig-zag-decode32 (v)
  (logxor (ash v -1) (- (logand v 1))))

(declaim (ftype (function (int64) uint64) zig-zag-encode64)
         (inline zig-zag-encode64))

(defun zig-zag-encode64 (v)
  (logxor (ash v 1) (ash v -63)))

(declaim (ftype (function (uint64) int64) zig-zag-decode64)
         (inline zig-zag-decode64))

(defun zig-zag-decode64 (v)
  (logxor (ash v -1) (- (logand v 1))))

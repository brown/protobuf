;;;; Copyright 2011 Google Inc.  All Rights Reserved

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

;;;; Variable length encoding for integers.

(in-package #:varint)

(defconst +max-octets-32+ 5 "Maximum number of octets needed to encode a 32-bit integer.")
(defconst +max-octets-64+ 10 "Maximum number of octets needed to encode a 64-bit integer.")

;;; XXXX: Do we really need this many conditions?  Maybe eliminate
;;; varint-error, encoding-error, and/or parsing-error.

(define-condition varint-error (error)
  ()
  (:documentation "Superclass of all VARINT conditions."))

(define-condition encoding-error (varint-error)
  ()
  (:documentation "Superclass of all VARINT encoding conditions."))

(define-condition buffer-overflow (encoding-error)
  ()
  (:documentation "Buffer space exhausted while encoding a value."))

(define-condition parsing-error (varint-error)
  ()
  (:documentation "Superclass of all VARINT decoding conditions."))

(define-condition data-exhausted (parsing-error)
  ()
  (:documentation "Decoding a value requires more data than is available."))

(define-condition value-out-of-range (parsing-error)
  ()
  (:documentation "Value decoded is outside the range of the return type."))

(define-condition alignment (parsing-error)
  ()
  (:documentation "Data buffer does not contain the type of value we have
been asked to skip over or parse backwards."))

(declaim (ftype (function (octet-vector vector-index uint32) (values vector-index &optional))
                encode-uint32))

(defun encode-uint32 (buffer index v)
  "Encode V, an unsigned 32-bit integer, into BUFFER at INDEX."
  (declare (type octet-vector buffer)
           (type vector-index index)
           (type uint32 v))
  (cond ((< v (ash 1 7))
         (setf (aref buffer index) v)
         (incf index))
        ((< v (ash 1 14))
         (setf (nibbles:ub16ref/le buffer index)
               (logior #x80
                       (ldb (byte 7 0) v)
                       (ash (ldb (byte 7 7) v) 8)))
         (incf index 2))
        ((< v (ash 1 21))
         (setf (nibbles:ub16ref/le buffer index)
               (logior #x8080
                       (ldb (byte 7 0) v)
                       (ash (ldb (byte 7 7) v) 8)))
         (incf index 2)
         (setf (aref buffer index) (ldb (byte 7 14) v))
         (incf index))
        ((< v (ash 1 28))
         (setf (nibbles:ub32ref/le buffer index)
               (logior #x808080
                       (ldb (byte 7 0) v)
                       (ash (ldb (byte 7 7) v) 8)
                       (ash (ldb (byte 7 14) v) 16)
                       (ash (ldb (byte 7 21) v) 24)))
         (incf index 4))
        (t (setf (nibbles:ub32ref/le buffer index)
                 (logior #x80808080
                         (ldb (byte 7 0) v)
                         (ash (ldb (byte 7 7) v) 8)
                         (ash (ldb (byte 7 14) v) 16)
                         (ash (ldb (byte 7 21) v) 24)))
           (incf index 4)
           (setf (aref buffer index) (ldb (byte 4 28) v))
           (incf index)))
  index)

(declaim (ftype (function (octet-vector vector-index vector-index uint32)
                          (values vector-index &optional))
                encode-uint32-carefully))

(defun encode-uint32-carefully (buffer index limit v)
  "Encode V, an unsigned 32-bit integer, into BUFFER at INDEX, taking care
to never write past position LIMIT.  If writing past LIMIT is required to
encode V, then raise ENCODE-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type uint32 v))
  (cond ((< v (ash 1 7))
         (when (>= index limit) (error 'buffer-overflow))
         (setf (aref buffer index) v)
         (incf index))
        ((< v (ash 1 14))
         (when (>= (1+ index) limit) (error 'buffer-overflow))
         (setf (nibbles:ub16ref/le buffer index)
               (logior #x80
                       (ldb (byte 7 0) v)
                       (ash (ldb (byte 7 7) v) 8)))
         (incf index 2))
        ((< v (ash 1 21))
         (when (>= (+ index 2) limit) (error 'buffer-overflow))
         (setf (nibbles:ub16ref/le buffer index)
               (logior #x8080
                       (ldb (byte 7 0) v)
                       (ash (ldb (byte 7 7) v) 8)))
         (incf index 2)
         (setf (aref buffer index) (ldb (byte 7 14) v))
         (incf index))
        ((< v (ash 1 28))
         (when (>= (+ index 3) limit) (error 'buffer-overflow))
         (setf (nibbles:ub32ref/le buffer index)
               (logior #x808080
                       (ldb (byte 7 0) v)
                       (ash (ldb (byte 7 7) v) 8)
                       (ash (ldb (byte 7 14) v) 16)
                       (ash (ldb (byte 7 21) v) 24)))
         (incf index 4))
        (t (when (>= (+ index 4) limit) (error 'buffer-overflow))
           (setf (nibbles:ub32ref/le buffer index)
                 (logior #x80808080
                         (ldb (byte 7 0) v)
                         (ash (ldb (byte 7 7) v) 8)
                         (ash (ldb (byte 7 14) v) 16)
                         (ash (ldb (byte 7 21) v) 24)))
           (incf index 4)
           (setf (aref buffer index) (ldb (byte 4 28) v))
           (incf index)))
  index)

(declaim (ftype (function (octet-vector vector-index uint64) (values vector-index &optional))
                encode-uint64))

(defun encode-uint64 (buffer index v)
  "Encode V, an unsigned 64-bit integer, into BUFFER at INDEX."
  (declare (type octet-vector buffer)
           (type vector-index index)
           (type uint64 v))
  (loop do (let ((bits (ldb (byte 8 0) v)))
             (setf v (ash v -7))
             (setf (aref buffer index) (logior bits (if (not (zerop v)) 128 0)))
             (incf index))
        until (zerop v))
  index)

(declaim (ftype (function (octet-vector vector-index vector-index uint64)
                          (values vector-index &optional))
                encode-uint64-carefully))

(defun encode-uint64-carefully (buffer index limit v)
  "Encode V, an unsigned 64-bit integer, into BUFFER at INDEX, taking care
to never write past position LIMIT.  If writing past LIMIT is required to
encode V, then raise BUFFER-OVERFLOW."
  (declare (type octet-vector buffer)
           (type vector-index index limit)
           (type uint64 v))
  (loop do (let ((bits (ldb (byte 8 0) v)))
             (setf v (ash v -7))
             (when (>= index limit) (error 'buffer-overflow))
             (setf (aref buffer index) (logior bits (if (not (zerop v)) 128 0)))
             (incf index))
        until (zerop v))
  index)

(declaim (ftype (function (octet-vector vector-index) (values uint32 vector-index &optional))
                parse-uint32))

(defun parse-uint32 (buffer index)
  (declare (type octet-vector buffer)
           (type vector-index index))
  (prog* ((byte (prog1 (aref buffer index) (incf index)))
          (result (ldb (byte 7 0) byte)))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 7) result) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 14) result) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 21) result) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 4 28) result) (ldb (byte 4 0) byte))
     (when (< byte 128) (go done))
     (error 'value-out-of-range)
   DONE
     (return (values result index))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint32 vector-index &optional))
                parse-uint32-carefully))

(defun parse-uint32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (if (<= (+ index +max-octets-32+) limit)
      (parse-uint32 buffer index)
      (progn
        (when (>= index limit)
          (error 'data-exhausted))
        (prog* ((byte (aref buffer index))
                (result (ldb (byte 7 0) byte)))
           (incf index)
           (when (< byte 128) (go done))
           (when (>= index limit) (go bad))
           (setf byte (prog1 (aref buffer index) (incf index)))
           (setf (ldb (byte 7 7) result) (ldb (byte 7 0) byte))
           (when (< byte 128) (go done))
           (when (>= index limit) (go bad))
           (setf byte (prog1 (aref buffer index) (incf index)))
           (setf (ldb (byte 7 14) result) (ldb (byte 7 0) byte))
           (when (< byte 128) (go done))
           (when (>= index limit) (go bad))
           (setf byte (prog1 (aref buffer index) (incf index)))
           (setf (ldb (byte 7 21) result) (ldb (byte 7 0) byte))
           (when (< byte 128) (go done))
           (when (>= index limit) (go bad))
           (setf byte (prog1 (aref buffer index) (incf index)))
           (setf (ldb (byte 4 28) result) (ldb (byte 4 0) byte))
           (when (< byte 128) (go done))
           (error 'value-out-of-range)
         BAD
           (error 'data-exhausted)
         DONE
           (return (values result index))))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values (unsigned-byte 31) vector-index &optional))
                parse-uint31-carefully))

(defun parse-uint31-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (multiple-value-bind (result new-index)
      (parse-uint32-carefully buffer index limit)
    ;; Ensure result fits in 31 bits.
    (when (logbitp 31 result) (error 'value-out-of-range))
    (values result new-index)))

(declaim (ftype (function (octet-vector vector-index) (values uint64 vector-index &optional))
                parse-uint64))

(defun parse-uint64 (buffer index)
  (declare (type octet-vector buffer)
           (type vector-index index))
  (prog* ((byte (prog1 (aref buffer index) (incf index)))
          (result1 (ldb (byte 7 0) byte))
          (result2 0)
          (result3 0))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 7) result1) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 14) result1) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 21) result1) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))

     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf result2 (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 7) result2) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 14) result2) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 21) result2) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))

     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf result3 (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 1 7) result3) (ldb (byte 1 0) byte))
     (when (< byte 128) (go done))

     (error 'value-out-of-range)
   DONE
     (return (values (logior result1 (ash result2 28) (ash result3 56))
                     index))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint64 vector-index &optional))
                parse-uint64-carefully))

(defun parse-uint64-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (when (>= index limit) (error 'data-exhausted))
  (prog* ((byte (prog1 (aref buffer index) (incf index)))
          (result1 (ldb (byte 7 0) byte))
          (result2 0)
          (result3 0))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 7) result1) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 14) result1) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 21) result1) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))

     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf result2 (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 7) result2) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 14) result2) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 7 21) result2) (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))

     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf result3 (ldb (byte 7 0) byte))
     (when (< byte 128) (go done))
     (when (>= index limit) (go bad))
     (setf byte (prog1 (aref buffer index) (incf index)))
     (setf (ldb (byte 1 7) result3) (ldb (byte 1 0) byte))
     (when (< byte 128) (go done))
     (error 'value-out-of-range)

   BAD
     (error 'data-exhausted)
   DONE
     (return (values (logior result1 (ash result2 28) (ash result3 56))
                     index))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values int64 vector-index &optional))
                parse-int64-carefully))

(defun parse-int64-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (multiple-value-bind (result new-index)
      (parse-uint64-carefully buffer index limit)
    (when (logbitp 63 result)           ; sign bit set, so value is negative
      (decf result (ash 1 64)))
    (values result new-index)))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values int32 vector-index &optional))
                parse-int32-carefully))

(defun parse-int32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (multiple-value-bind (result new-index)
      ;; XXXX Optimize this, since we are only interested in the low 32 bits.
      (parse-uint64-carefully buffer index limit)
    (setf result (ldb (byte 32 0) result))
    (when (logbitp 31 result)           ; sign bit set, so value is negative
      (decf result (ash 1 32)))
    (values result new-index)))

(declaim (ftype (function (octet-vector vector-index vector-index) (values vector-index &optional))
                skip32-carefully))

(defun skip32-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (prog ()
     (when (> (+ index +max-octets-32+) limit) (go carefully))

     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
   value-out-of-range
     (error 'value-out-of-range)

   carefully
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (go value-out-of-range)

   data-exhausted
     (error 'data-exhausted)
   done
     (return (1+ index))))

(declaim (ftype (function (octet-vector vector-index vector-index) (values vector-index &optional))
                skip64-carefully))

(defun skip64-carefully (buffer index limit)
  (declare (type octet-vector buffer)
           (type vector-index index limit))
  (prog ()
     (when (> (+ index +max-octets-64+) limit) (go carefully))

     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (< (aref buffer index) 128) (go done))
   value-out-of-range
     (error 'value-out-of-range)

   carefully
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (incf index)
     (when (>= index limit) (go data-exhausted))
     (when (< (aref buffer index) 128) (go done))
     (go value-out-of-range)

   data-exhausted
     (error 'data-exhausted)
   done
     (return (1+ index))))

(declaim (ftype (function (octet-vector vector-index vector-index) (values vector-index &optional))
                skip32-backward-slow))

(defun skip32-backward-slow (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (assert (>= index base))
  (when (or (= index base)
            (> (aref buffer (decf index)) 127))
    (error 'alignment))
  (dotimes (i +max-octets-32+)
    (when (= index base)
      (return-from skip32-backward-slow index))
    (when (< (aref buffer (decf index)) 128)
      (return-from skip32-backward-slow (1+ index))))
  (error 'alignment))

(declaim (ftype (function (octet-vector vector-index vector-index) (values vector-index &optional))
                skip64-backward-slow))

(defun skip64-backward-slow (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (assert (>= index base))
  (when (or (= index base)
            (> (aref buffer (decf index)) 127))
    (error 'alignment))
  (dotimes (i +max-octets-64+)
    (when (= index base)
      (return-from skip64-backward-slow index))
    (when (< (aref buffer (decf index)) 128)
      (return-from skip64-backward-slow (1+ index))))
  (error 'alignment))

(declaim (ftype (function (octet-vector vector-index vector-index) (values vector-index &optional))
                skip32-backward))

(defun skip32-backward (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (if (<= index (+ base +max-octets-32+))
      (skip32-backward-slow buffer index base)
      (prog ()
         (when (> (aref buffer (decf index)) 127) (go bad))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
       BAD
         (error 'value-out-of-range)
       DONE
         (return (1+ index)))))

(declaim (ftype (function (octet-vector vector-index vector-index) (values vector-index &optional))
                skip64-backward))

(defun skip64-backward (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (if (<= index (+ base +max-octets-64+))
      (skip64-backward-slow buffer index base)
      (prog ()
         (when (> (aref buffer (decf index)) 127) (go bad))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
         (when (< (aref buffer (decf index)) 128) (go done))
       BAD
         (error 'value-out-of-range)
       DONE
         (return (1+ index)))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint32 vector-index &optional))
                parse32-backward-slow))

(defun parse32-backward-slow (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (let ((prev (skip32-backward-slow buffer index base)))
    (values (parse-uint32 buffer prev) prev)))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint64 vector-index &optional))
                parse64-backward-slow))

(defun parse64-backward-slow (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (let ((prev (skip64-backward-slow buffer index base)))
    (values (parse-uint64 buffer prev) prev)))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint32 vector-index &optional))
                parse32-backward))

(defun parse32-backward (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (if (<= index (+ base +max-octets-32+))
      (parse32-backward-slow buffer index base)
      (let ((byte (aref buffer (decf index))))
        (when (logbitp 7 byte) (error 'alignment))
        (prog ((result (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))

           (when (>= result (ash 1 25)) (error 'value-out-of-range))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (error 'value-out-of-range)

         DONE
           (return (values result (1+ index)))))))

(declaim (ftype (function (octet-vector vector-index vector-index)
                          (values uint64 vector-index &optional))
                parse64-backward))

(defun parse64-backward (buffer index base)
  (declare (type octet-vector buffer)
           (type vector-index index base))
  (if (<= index (+ base +max-octets-64+))
      (parse64-backward-slow buffer index base)
      (let ((byte (aref buffer (decf index))))
        (when (> byte 127) (error 'alignment))
        (prog ((result (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))

           (when (>= result (ash 1 57)) (error 'value-out-of-range))
           (setf result (logior (ash result 7) (ldb (byte 7 0) byte)))
           (setf byte (aref buffer (decf index)))
           (when (not (logbitp 7 byte)) (go done))
           (error 'value-out-of-range)

         DONE
           (return (values result (1+ index)))))))

(declaim (ftype (function (uint32) (values (integer 1 #.+max-octets-32+) &optional)) length-uint32))

(defun length-uint32 (v)
  (declare (type uint32 v))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint32 1))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint32 2))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint32 3))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint32 4))
  5)

(declaim (ftype (function (int32) (values (integer 1 #.+max-octets-64+) &optional)) length-int32))

(defun length-int32 (v)
  (declare (type int32 v))
  (if (minusp v)
      +max-octets-64+
      (length-uint32 (ldb (byte 32 0) v))))

(declaim (ftype (function (uint64) (values (integer 1 #.+max-octets-64+) &optional)) length-uint64))

(defun length-uint64 (v)
  (declare (type uint64 v))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 1))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 2))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 3))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 4))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 5))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 6))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 7))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 8))
  (setf v (ash v -7))
  (when (zerop v) (return-from length-uint64 9))
  10)

;; Temporary compatibility functions to unblock a Quicklisp release.  XXXXXXXXXX Delete soon.
(declaim (ftype (function (uint32) (values (integer 1 5) &optional)) length32))
(defun length32 (v)
  (declare (type uint32 v))
  (length-uint32 v))
(declaim (ftype (function (uint64) (values (integer 1 10) &optional)) length64))
(defun length64 (v)
  (declare (type uint64 v))
  (length-uint64 v))


;;;;    base.lisp


;; Copyright 2010, Google Inc.
;; All rights reserved.

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


(in-package #:base)

(declaim #.optimize:+default+)


;;; DEFCONST is equivalent to DEFCONSTANT, but never defines a constant more
;;; than once.  We use DEFCONST because SBCL warns when a constant is
;;; redefined to a value not EQ to its original value.

(defmacro defconst (name form &optional (doc-string nil))
  "Define global constant NAME as holding the result of evaluating FORM.  When
DOC-STRING is supplied, make it the constant's documentation."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp ',name)
       ,(if doc-string
            `(defconstant ,name ,form ,doc-string)
            `(defconstant ,name ,form)))))


;;; Lisp integer types with the same numeric range as C++ ints.

(deftype int32 () '(signed-byte 32))
(deftype int64 () '(signed-byte 64))
(deftype uint32 () '(unsigned-byte 32))
(deftype uint64 () '(unsigned-byte 64))


;;; Octet vectors

(deftype vector-index () '(integer 0 #.(1- array-dimension-limit)))

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype octet-vector-index () '(integer 0 #.(1- array-dimension-limit)))

(defconst +octet-vector-index-bits+
  (integer-length (1- array-dimension-limit)))
(defconst +illegal-octet-vector-index+ (1- array-dimension-limit))


(declaim (ftype (function (fixnum &key (:initial-contents list)) octet-vector)
                make-octet-vector))

(defun make-octet-vector (octet-count &key initial-contents)
  "Create an octet vector containing OCTET-COUNT octets.  If INITIAL-CONTENTS
is not supplied, each element of the vector is initialized to zero.  Otherwise,
the vector is initialized to the contents of list INITIAL-CONTENTS."
  (declare (type octet-vector-index octet-count)
           (type list initial-contents))
  (if initial-contents
      (make-array octet-count
                  :element-type 'octet
                  :initial-contents initial-contents)
      (make-array octet-count :element-type 'octet :initial-element 0)))

(declaim (ftype (function (string
                           &key (:start vector-index) (:end vector-index))
                          octet-vector)
                string-to-utf8-octets))

(defun string-to-utf8-octets (string &key (start 0) (end (length string)))
  "Convert STRING into an octet-vector by uft-8 encoding each character."
  (declare (type string string)
           (type vector-index start end)
           (optimize (speed 3) (safety 0)))
  #+allegro
  (excl:string-to-octets string
                         :start start :end end
                         :null-terminate nil :external-format :utf8)
  #+clisp
  (ext:convert-string-to-bytes string charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:string-to-octets string :start start :end end)
  #-(or allegro clisp sbcl)
  (trivial-utf-8:string-to-utf-8-bytes (subseq string start end)))

(declaim (ftype (function (octet-vector
                           &key (:start vector-index) (:end vector-index))
                          string)
                utf8-octets-to-string))

(defun utf8-octets-to-string (octets &key (start 0) (end (length octets)))
  "Convert utf-8 encoded OCTETS into a string."
  (declare (type octet-vector octets)
           (type vector-index start end)
           (optimize (speed 3) (safety 0)))
  #+allegro
  (excl:octets-to-string octets :start start :end end :external-format :utf8)
  #+clisp
  (ext:convert-string-from-bytes octets charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:octets-to-string octets :start start :end end :external-format :utf8)
  #-(or allegro clisp sbcl)
  (trivial-utf-8:utf-8-bytes-to-string (subseq octets start end)))

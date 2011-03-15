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

;;;; Author: brown@google.com (Robert Brown)

;;;; Vectors of octets, 8-bit bytes, used to store UTF-8 encoded strings.

(in-package #:base)
(declaim #.*optimize-fast-unsafe*)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))

(declaim (ftype (function (fixnum &key (:initial-contents list)) (values octet-vector &optional))
                make-octet-vector))

(defun make-octet-vector (octet-count &key initial-contents)
  "Create an OCTET-VECTOR containing OCTET-COUNT octets.  If INITIAL-CONTENTS
is not supplied, each element of the vector is initialized to zero.  Otherwise,
the vector is initialized to the contents of list INITIAL-CONTENTS."
  (declare (type vector-index octet-count)
           (type list initial-contents))
  (if initial-contents
      (make-array octet-count :element-type 'octet :initial-contents initial-contents)
      (make-array octet-count :element-type 'octet :initial-element 0)))

(declaim (ftype (function (string &key (:start vector-index) (:end vector-index))
                          (values octet-vector &optional))
                string-to-utf8-octets))

(defun string-to-utf8-octets (string &key (start 0) (end (length string)))
  "Convert STRING into an OCTET-VECTOR by UTF-8 encoding each character."
  (declare (type string string)
           (type vector-index start end))
  #+allegro
  (excl:string-to-octets string :start start :end end :null-terminate nil :external-format :utf8)
  #+ccl
  (ccl:encode-string-to-octets string :start start :end end :external-format :utf-8)
  #+clisp
  (ext:convert-string-to-bytes string charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:string-to-octets string :start start :end end)
  #-(or allegro ccl clisp sbcl)
  (trivial-utf-8:string-to-utf-8-bytes (subseq string start end)))

(declaim (ftype (function (octet-vector &key (:start vector-index) (:end vector-index))
                          (values string &optional))
                utf8-octets-to-string))

(defun utf8-octets-to-string (octets &key (start 0) (end (length octets)))
  "Convert OCTETS, a vector of UTF-8 encoded octets, into a string."
  (declare (type octet-vector octets)
           (type vector-index start end))
  #+allegro
  (excl:octets-to-string octets :start start :end end :external-format :utf8)
  #+ccl
  (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8)
  #+clisp
  (ext:convert-string-from-bytes octets charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:octets-to-string octets :start start :end end :external-format :utf8)
  #-(or allegro ccl clisp sbcl)
  (trivial-utf-8:utf-8-bytes-to-string (subseq octets start end)))

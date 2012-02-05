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

;;;; Test protocol buffer messages.

(in-package #:common-lisp-user)

(defpackage #:message-test
  (:documentation "Tests for protocol buffer messages.")
  (:use #:common-lisp
        #:com.google.base
        #:com.google.protobuf.test
        #:hu.dwim.stefil
        #:protobuf-test-config
        #:unittest-proto)
  (:export #:test-message))

(in-package #:message-test)
(declaim #.*optimize-default*)

(defsuite (test-message :in root-suite) ()
  (run-child-tests))

(in-suite test-message)

(defconst +golden-file-name+
  (merge-pathnames "google/protobuf/testdata/golden_message" *base-directory*))

(defconst +golden-packed-file-name+
  (merge-pathnames "google/protobuf/testdata/golden_packed_fields_message" *base-directory*))

(defun sf (string)
  "Converts STRING into a protocol buffer string field value."
  (pb:string-field string))

(defun bf (string)
  "Converts STRING into a protocol buffer byte field value."
  (string-to-utf8-octets string))

(defvar *optional-field-info*
  ;; field name, default value, value set by tests
  `((optional-int32 0 101) (optional-int64 0 102)
    (optional-uint32 0 103) (optional-uint64 0 104)
    (optional-sint32 0 105) (optional-sint64 0 106)
    (optional-fixed32 0 107) (optional-fixed64 0 108)
    (optional-sfixed32 0 109) (optional-sfixed64 0 110)
    (optional-float 0f0 111f0) (optional-double 0d0 112d0)
    (optional-bool nil t)
    (optional-string ,(sf "") ,(sf "115"))
    (optional-bytes ,(bf "") ,(bf "116"))
    (optional-nested-enum ,+test-all-types-nested-enum-foo+ ,+test-all-types-nested-enum-baz+)
    (optional-foreign-enum ,+foreign-enum-foreign-foo+ ,+foreign-enum-foreign-baz+)
    (optional-import-enum ,+import-enum-import-foo+ ,+import-enum-import-baz+)
    ;; XXXX: C++ test does not verify these fields.
    (optional-string-piece ,(sf "") ,(sf "124"))
    (optional-cord ,(sf "") ,(sf "125"))))

(defvar *default-field-info*
  ;; field name, default value, value set by tests
  `((default-int32 41 401) (default-int64 42 402)
    (default-uint32 43 403) (default-uint64 44 404)
    (default-sint32 -45 405) (default-sint64 46 406)
    (default-fixed32 47 407) (default-fixed64 48 408)
    (default-sfixed32 49 409) (default-sfixed64 -50 410)
    (default-float 51.5f0 411f0) (default-double 52d3 412d0)
    (default-bool t nil)
    (default-string ,(sf "hello") ,(sf "415"))
    (default-bytes ,(bf "world") ,(bf "416"))
    (default-nested-enum ,+test-all-types-nested-enum-bar+ ,+test-all-types-nested-enum-foo+)
    (default-foreign-enum ,+foreign-enum-foreign-bar+ ,+foreign-enum-foreign-foo+)
    (default-import-enum ,+import-enum-import-bar+ ,+import-enum-import-foo+)
    ;; XXXX: C++ test does not verify these fields.
    (default-string-piece ,(sf "abc") ,(sf "424"))
    (default-cord ,(sf "123") ,(sf "425"))))

(defvar *repeated-field-info*
  ;; field name, default value, value set by tests, modification value
  `((repeated-int32 201 301 501) (repeated-int64 202 302 502)
    (repeated-uint32 203 303 503) (repeated-uint64 204 304 504)
    (repeated-sint32 205 305 505) (repeated-sint64 206 306 506)
    (repeated-fixed32 207 307 507) (repeated-fixed64 208 308 508)
    (repeated-sfixed32 209 309 509) (repeated-sfixed64 210 310 510)
    (repeated-float 211f0 311f0 511f0) (repeated-double 212d0 312d0 512d0)
    (repeated-bool t nil t)
    (repeated-string ,(sf "215") ,(sf "315") ,(sf "515"))
    (repeated-bytes ,(bf "216") ,(bf "316") ,(bf "516"))
    (repeated-nested-enum
     ,+test-all-types-nested-enum-bar+
     ,+test-all-types-nested-enum-baz+
     ,+test-all-types-nested-enum-foo+)
    (repeated-foreign-enum
     ,+foreign-enum-foreign-bar+
     ,+foreign-enum-foreign-baz+
     ,+foreign-enum-foreign-foo+)
    (repeated-import-enum
     ,+import-enum-import-bar+
     ,+import-enum-import-baz+
     ,+import-enum-import-foo+)
    ;; XXXX: C++ test does not verify these fields.
    (repeated-string-piece ,(sf "224") ,(sf "324") ,(sf "524"))
    (repeated-cord ,(sf "225") ,(sf "325") ,(sf "525"))))

(defun field-equal (value expected)
  (cond ((eq (type-of expected) 'pb::%sf%)
         (is (string= (pb:string-value value) (pb:string-value expected))))
        ((vectorp value) (is (equalp value expected)))
        (t (is (eql value expected)))))

(defun field-function (prefix field)
  (let ((package (find-package 'unittest-proto))
        (symbol-name (concatenate 'string prefix (symbol-name field))))
    (symbol-function (find-symbol symbol-name package))))

(defun field-setter (field)
  (let ((package (find-package 'unittest-proto)))
    (fdefinition `(setf ,(find-symbol (symbol-name field) package)))))

(defun expect-all-fields-set (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
      (let ((has (field-function "HAS-" field))
            (accessor (field-function "" field))
            (value (second values)))
        (is (funcall has m))
        (field-equal (funcall accessor m) value))))

  (is (has-optional-group m))
  (is (has-a (optional-group m)))
  (is (= (a (optional-group m)) 117))

  (is (has-optional-nested-message m))
  (is (has-bb (optional-nested-message m)))
  (is (= (bb (optional-nested-message m)) 118))

  (is (has-optional-foreign-message m))
  (is (has-c (optional-foreign-message m)))
  (is (= (c (optional-foreign-message m)) 119))

  (is (has-optional-import-message m))
  (is (has-d (optional-import-message m)))
  (is (= (d (optional-import-message m)) 120))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function "" field))
            (v0 (first values))
            (v1 (second values)))
        (is (= (length (funcall accessor m)) 2))
        (field-equal (aref (funcall accessor m) 0) v0)
        (field-equal (aref (funcall accessor m) 1) v1))))
  (let ((v (repeated-group m)))
    (is (= (length v) 2))
    (is (= (a (aref v 0)) 217))
    (is (= (a (aref v 1)) 317)))
  (let ((v (repeated-nested-message m)))
    (is (= (length v) 2))
    (is (= (bb (aref v 0)) 218))
    (is (= (bb (aref v 1)) 318)))
  (let ((v (repeated-foreign-message m)))
    (is (= (length v) 2))
    (is (= (c (aref v 0)) 219))
    (is (= (c (aref v 1)) 319)))
  (let ((v (repeated-import-message m)))
    (is (= (length v) 2))
    (is (= (d (aref v 0)) 220))
    (is (= (d (aref v 1)) 320))))

(defvar *packed-field-info*
  `((packed-int32 601 701) (packed-int64 602 702)
    (packed-uint32 603 703) (packed-uint64 604 704)
    (packed-sint32 605 705) (packed-sint64 606 706)
    (packed-fixed32 607 707) (packed-fixed64 608 708)
    (packed-sfixed32 609 709) (packed-sfixed64 610 710)
    (packed-float 611f0 711f0) (packed-double 612d0 712d0)
    (packed-bool t nil)
    (packed-enum ,+foreign-enum-foreign-bar+ ,+foreign-enum-foreign-baz+)))

(defun expect-packed-fields-set (m)
  (loop for (field . values) in *packed-field-info* do
    (let ((accessor (field-function "" field))
          (v0 (first values))
          (v1 (second values)))
      (is (= (length (funcall accessor m)) 2))
      (field-equal (aref (funcall accessor m) 0) v0)
      (field-equal (aref (funcall accessor m) 1) v1))))

(defun read-message (class-name file-name)
  (let ((message (make-instance class-name)))
    (with-open-file (input file-name :direction :input :element-type 'unsigned-byte)
      (let* ((size (file-length input))
             (buffer (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence buffer input)
        (pb:merge-from-array message buffer 0 size)))
    message))

(deftest test-parse-from-file ()
  (let ((message (read-message 'test-all-types +golden-file-name+)))
    (expect-all-fields-set message)))

(deftest test-parse-packed-from-file ()
  (let ((message (read-message 'test-packed-types +golden-packed-file-name+)))
    (expect-packed-fields-set message)))

(defun set-all-fields (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
      (let ((setter (field-setter field))
            (value (second values)))
        (funcall setter value m))))
  (setf (a (optional-group m)) 117)
  (setf (bb (optional-nested-message m)) 118)
  (setf (c (optional-foreign-message m)) 119)
  (setf (d (optional-import-message m)) 120)

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function "" field))
            (v0 (first values))
            (v1 (second values)))
        (vector-push-extend v0 (funcall accessor m))
        (vector-push-extend v1 (funcall accessor m)))))
  (let ((v0 (make-instance 'test-all-types-repeated-group))
        (v1 (make-instance 'test-all-types-repeated-group)))
    (setf (a v0) 217)
    (setf (a v1) 317)
    (vector-push-extend v0 (repeated-group m))
    (vector-push-extend v1 (repeated-group m)))
  (let ((v0 (make-instance 'test-all-types-nested-message))
        (v1 (make-instance 'test-all-types-nested-message)))
    (setf (bb v0) 218)
    (setf (bb v1) 318)
    (vector-push-extend v0 (repeated-nested-message m))
    (vector-push-extend v1 (repeated-nested-message m)))
  (let ((v0 (make-instance 'foreign-message))
        (v1 (make-instance 'foreign-message)))
    (setf (c v0) 219)
    (setf (c v1) 319)
    (vector-push-extend v0 (repeated-foreign-message m))
    (vector-push-extend v1 (repeated-foreign-message m)))
  (let ((v0 (make-instance 'import-message))
        (v1 (make-instance 'import-message)))
    (setf (d v0) 220)
    (setf (d v1) 320)
    (vector-push-extend v0 (repeated-import-message m))
    (vector-push-extend v1 (repeated-import-message m))))

(deftest test-parse-helpers ()
  (let ((m1 (make-instance 'test-all-types))
        (m2 (make-instance 'test-all-types)))
    (set-all-fields m1)
    (expect-all-fields-set m1)
    (let* ((size (pb:octet-size m1))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (pb:serialize m1 buffer 0 size)
      (pb:merge-from-array m2 buffer 0 size)
      (expect-all-fields-set m2))))

(defun expect-clear (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
      (let ((has (field-function "HAS-" field))
            (accessor (field-function "" field))
            (default-value (first values)))
        (is (not (funcall has m)))
        (field-equal (funcall accessor m) default-value))))

  (is (not (has-optional-group m)))
  (is (not (has-a (optional-group m))))
  (is (= (a (optional-group m)) 0))

  (is (not (has-optional-nested-message m)))
  (is (not (has-bb (optional-nested-message m))))
  (is (= (bb (optional-nested-message m)) 0))

  (is (not (has-optional-foreign-message m)))
  (is (not (has-c (optional-foreign-message m))))
  (is (= (c (optional-foreign-message m)) 0))

  (is (not (has-optional-import-message m)))
  (is (not (has-d (optional-import-message m))))
  (is (= (d (optional-import-message m)) 0))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . nil) in field-info do
      (let ((accessor (field-function "" field)))
        (is (zerop (length (funcall accessor m))))))))

(defun modify-repeated-fields (m)
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function "" field))
            (v (third values)))
        (setf (aref (funcall accessor m) 1) v))))
  (setf (a (aref (repeated-group m) 1)) 517)
  (setf (bb (aref (repeated-nested-message m) 1)) 518)
  (setf (c (aref (repeated-foreign-message m) 1)) 519)
  (setf (d (aref (repeated-import-message m) 1)) 520))

(defun expect-repeated-fields-modified (m)
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
      (let ((accessor (field-function "" field))
            (v0 (first values))
            (v1 (third values)))
        (is (= (length (funcall accessor m)) 2))
        (field-equal (aref (funcall accessor m) 0) v0)
        (field-equal (aref (funcall accessor m) 1) v1))))
  (let ((v (repeated-group m)))
    (is (= (length v) 2))
    (is (= (a (aref v 0)) 217))
    (is (= (a (aref v 1)) 517)))
  (let ((v (repeated-nested-message m)))
    (is (= (length v) 2))
    (is (= (bb (aref v 0)) 218))
    (is (= (bb (aref v 1)) 518)))
  (let ((v (repeated-foreign-message m)))
    (is (= (length v) 2))
    (is (= (c (aref v 0)) 219))
    (is (= (c (aref v 1)) 519)))
  (let ((v (repeated-import-message m)))
    (is (= (length v) 2))
    (is (= (d (aref v 0)) 220))
    (is (= (d (aref v 1)) 520))))

(deftest test-modify-repeated-fields ()
  (let ((m (make-instance 'test-all-types)))
    (expect-clear m)
    (set-all-fields m)
    (expect-all-fields-set m)
    (modify-repeated-fields m)
    (expect-repeated-fields-modified m)
    (pb:clear m)
    (expect-clear m)))

(deftest test-serialize-and-merge ()
  (let ((m1 (make-instance 'test-all-types))
        (m2 (make-instance 'test-all-types))
        (m3 (make-instance 'test-all-types)))
    (set-all-fields m1)
    (pb:clear m2)
    (pb:merge-from-message m2 m1)
    (let* ((size (pb:octet-size m1))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (pb:serialize m1 buffer 0 size)
      (pb:merge-from-array m3 buffer 0 size))
    (expect-all-fields-set m2)
    (expect-all-fields-set m3)))

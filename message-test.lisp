
;;;;    message-test.lisp


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


(in-package #:message-test)

(declaim #.optimize:+default+)

(defconst +pwd+ (make-pathname
                 :directory (pathname-directory
                             (or *compile-file-truename* *load-truename*))))

(defconst +golden-file-name+
  (merge-pathnames
   "google-protobuf/src/google/protobuf/testdata/golden_message" +pwd+))

(defconst +golden-packed-file-name+
  (merge-pathnames
   "google-protobuf/src/google/protobuf/testdata/golden_packed_fields_message"
   +pwd+))

(defparameter *optional-field-info*
  ;; field name, default value, value set by tests
  '((optional-int32 0 101) (optional-int64 0 102)
    (optional-uint32 0 103) (optional-uint64 0 104)
    (optional-sint32 0 105) (optional-sint64 0 106)
    (optional-fixed32 0 107) (optional-fixed64 0 108)
    (optional-sfixed32 0 109) (optional-sfixed64 0 110)
    (optional-float 0s0 111s0) (optional-double 0d0 112d0)
    (optional-bool nil t)
    (optional-string "" "115") (optional-bytes "" "116")
    (optional-nested-enum
     #.pb:+testalltypes-nestedenum-foo+ #.pb:+testalltypes-nestedenum-baz+)
    (optional-foreign-enum
     #.pb:+foreignenum-foreign-foo+ #.pb:+foreignenum-foreign-baz+)
    (optional-import-enum
     #.pb:+importenum-import-foo+ #.pb:+importenum-import-baz+)
    ;; XXXX: C++ test does not verify these fields.
    (optional-string-piece "" "124") (optional-cord "" "125")
    ))

(defparameter *default-field-info*
  ;; field name, default value, value set by tests
  '((default-int32 41 401) (default-int64 42 402)
    (default-uint32 43 403) (default-uint64 44 404)
    (default-sint32 -45 405) (default-sint64 46 406)
    (default-fixed32 47 407) (default-fixed64 48 408)
    (default-sfixed32 49 409) (default-sfixed64 -50 410)
    (default-float 51.5s0 411s0) (default-double 52d3 412d0)
    (default-bool t nil)
    (default-string "hello" "415") (default-bytes "world" "416")
    (default-nested-enum
     #.pb:+testalltypes-nestedenum-bar+ #.pb:+testalltypes-nestedenum-foo+)
    (default-foreign-enum
     #.pb:+foreignenum-foreign-bar+ #.pb:+foreignenum-foreign-foo+)
    (default-import-enum
     #.pb:+importenum-import-bar+ #.pb:+importenum-import-foo+)
    ;; XXXX: C++ test does not verify these fields.
    (default-string-piece "abc" "424") (default-cord "123" "425")
    ))

(defparameter *repeated-field-info*
  ;; field name, default value, value set by tests, modification value
  '((repeated-int32 201 301 501) (repeated-int64 202 302 502)
    (repeated-uint32 203 303 503) (repeated-uint64 204 304 504)
    (repeated-sint32 205 305 505) (repeated-sint64 206 306 506)
    (repeated-fixed32 207 307 507) (repeated-fixed64 208 308 508)
    (repeated-sfixed32 209 309 509) (repeated-sfixed64 210 310 510)
    (repeated-float 211s0 311s0 511s0) (repeated-double 212d0 312d0 512d0)
    (repeated-bool t nil t)
    (repeated-string
     #.(base:string-to-utf8-octets "215")
     #.(base:string-to-utf8-octets "315")
     #.(base:string-to-utf8-octets "515"))
    (repeated-bytes
     #.(base:string-to-utf8-octets "216")
     #.(base:string-to-utf8-octets "316")
     #.(base:string-to-utf8-octets "516"))
    (repeated-nested-enum
     #.pb:+testalltypes-nestedenum-bar+
     #.pb:+testalltypes-nestedenum-baz+
     #.pb:+testalltypes-nestedenum-foo+)
    (repeated-foreign-enum
     #.pb:+foreignenum-foreign-bar+
     #.pb:+foreignenum-foreign-baz+
     #.pb:+foreignenum-foreign-foo+)
    (repeated-import-enum
     #.pb:+importenum-import-bar+
     #.pb:+importenum-import-baz+
     #.pb:+importenum-import-foo+)
    ;; XXXX: C++ test does not verify these fields.
    (repeated-string-piece
     #.(base:string-to-utf8-octets "224")
     #.(base:string-to-utf8-octets "324")
     #.(base:string-to-utf8-octets "524"))
    (repeated-cord
     #.(base:string-to-utf8-octets "225")
     #.(base:string-to-utf8-octets "325")
     #.(base:string-to-utf8-octets "525"))
    ))

(defun field-equal (x y)
  (cond ((stringp x) (and (stringp y) (string= x y)))
        ((vectorp x) (equalp x y))
        (t (eql x y))))

(defun field-function (prefix field)
  (let ((symbol-name (concatenate 'string prefix (symbol-name field)))
        (package (find-package "PROTOCOL-BUFFER")))
    (symbol-function (find-symbol symbol-name package))))

(defun field-setter (field)
  (let ((package (find-package "PROTOCOL-BUFFER")))
    (fdefinition `(setf ,(find-symbol (symbol-name field) package)))))

(defun expect-all-fields-set (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
          (let ((has (field-function "HAS-" field))
                (accessor (field-function "" field))
                (value (second values)))
            (assert (funcall has m))
            (assert (field-equal (funcall accessor m) value)))))

  (assert (pb:has-optionalgroup m))
  (assert (pb:has-a (pb:optionalgroup m)))
  (assert (= (pb:a (pb:optionalgroup m)) 117))

  (assert (pb:has-optional-nested-message m))
  (assert (pb:has-bb (pb:optional-nested-message m)))
  (assert (= (pb:bb (pb:optional-nested-message m)) 118))

  (assert (pb:has-optional-foreign-message m))
  (assert (pb:has-c (pb:optional-foreign-message m)))
  (assert (= (pb:c (pb:optional-foreign-message m)) 119))

  (assert (pb:has-optional-import-message m))
  (assert (pb:has-d (pb:optional-import-message m)))
  (assert (= (pb:d (pb:optional-import-message m)) 120))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
          (let ((accessor (field-function "" field))
                (v0 (first values))
                (v1 (second values)))
            (assert (= (length (funcall accessor m)) 2))
            (assert (field-equal (aref (funcall accessor m) 0) v0))
            (assert (field-equal (aref (funcall accessor m) 1) v1)))))
  (let ((v (pb:repeatedgroup m)))
    (assert (= (length v) 2))
    (assert (= (pb:a (aref v 0)) 217))
    (assert (= (pb:a (aref v 1)) 317)))
  (let ((v (pb:repeated-nested-message m)))
    (assert (= (length v) 2))
    (assert (= (pb:bb (aref v 0)) 218))
    (assert (= (pb:bb (aref v 1)) 318)))
  (let ((v (pb:repeated-foreign-message m)))
    (assert (= (length v) 2))
    (assert (= (pb:c (aref v 0)) 219))
    (assert (= (pb:c (aref v 1)) 319)))
  (let ((v (pb:repeated-import-message m)))
    (assert (= (length v) 2))
    (assert (= (pb:d (aref v 0)) 220))
    (assert (= (pb:d (aref v 1)) 320))))

(defconst +packed-field-info+
  '((packed-int32 601 701) (packed-int64 602 702)
    (packed-uint32 603 703) (packed-uint64 604 704)
    (packed-sint32 605 705) (packed-sint64 606 706)
    (packed-fixed32 607 707) (packed-fixed64 608 708)
    (packed-sfixed32 609 709) (packed-sfixed64 610 710)
    (packed-float 611s0 711s0) (packed-double 612d0 712d0)
    (packed-bool t nil)
    (packed-enum
     #.pb:+foreignenum-foreign-bar+ #.pb:+foreignenum-foreign-baz+)))

(defun expect-packed-fields-set (m)
  (loop for (field . values) in +packed-field-info+ do
        (let ((accessor (field-function "" field))
              (v0 (first values))
              (v1 (second values)))
          (assert (= (length (funcall accessor m)) 2))
          (assert (field-equal (aref (funcall accessor m) 0) v0))
          (assert (field-equal (aref (funcall accessor m) 1) v1)))))

(defun read-message (class-name file-name)
  (let ((message (make-instance class-name)))
    (with-open-file (input file-name
                     :direction :input :element-type 'unsigned-byte)
      (let* ((size (file-length input))
             (buffer (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence buffer input)
        (pb:merge-from-array message buffer 0 size)))
    message))

(defun test-parse-from-file ()
  (let ((message (read-message 'pb:testalltypes +golden-file-name+)))
    (expect-all-fields-set message)))

(defun test-parse-packed-from-file ()
  (let ((message (read-message 'pb:testpackedtypes +golden-packed-file-name+)))
    (expect-packed-fields-set message)))

(defun set-all-fields (m)
  ;; optional and default fields
  (let ((field-info (append *optional-field-info* *default-field-info*)))
    (loop for (field . values) in field-info do
          (let ((setter (field-setter field))
                (value (second values)))
            (funcall setter value m))))
  (setf (pb:a (pb:optionalgroup m)) 117)
  (setf (pb:bb (pb:optional-nested-message m)) 118)
  (setf (pb:c (pb:optional-foreign-message m)) 119)
  (setf (pb:d (pb:optional-import-message m)) 120)

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
          (let ((accessor (field-function "" field))
                (v0 (first values))
                (v1 (second values)))
            (vector-push-extend v0 (funcall accessor m))
            (vector-push-extend v1 (funcall accessor m)))))
  (let ((v0 (make-instance 'pb:testalltypes-repeatedgroup))
        (v1 (make-instance 'pb:testalltypes-repeatedgroup)))
    (setf (pb:a v0) 217)
    (setf (pb:a v1) 317)
    (vector-push-extend v0 (pb:repeatedgroup m))
    (vector-push-extend v1 (pb:repeatedgroup m)))
  (let ((v0 (make-instance 'pb:testalltypes-nestedmessage))
        (v1 (make-instance 'pb:testalltypes-nestedmessage)))
    (setf (pb:bb v0) 218)
    (setf (pb:bb v1) 318)
    (vector-push-extend v0 (pb:repeated-nested-message m))
    (vector-push-extend v1 (pb:repeated-nested-message m)))
  (let ((v0 (make-instance 'pb:foreignmessage))
        (v1 (make-instance 'pb:foreignmessage)))
    (setf (pb:c v0) 219)
    (setf (pb:c v1) 319)
    (vector-push-extend v0 (pb:repeated-foreign-message m))
    (vector-push-extend v1 (pb:repeated-foreign-message m)))
  (let ((v0 (make-instance 'pb:importmessage))
        (v1 (make-instance 'pb:importmessage)))
    (setf (pb:d v0) 220)
    (setf (pb:d v1) 320)
    (vector-push-extend v0 (pb:repeated-import-message m))
    (vector-push-extend v1 (pb:repeated-import-message m))))

(defun test-parse-helpers ()
  (let ((m1 (make-instance 'pb:testalltypes))
        (m2 (make-instance 'pb:testalltypes)))
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
            (assert (not (funcall has m)))
            (assert (field-equal (funcall accessor m) default-value)))))

  (assert (not (pb:has-optionalgroup m)))
  (assert (not (pb:has-a (pb:optionalgroup m))))
  (assert (= (pb:a (pb:optionalgroup m)) 0))

  (assert (not (pb:has-optional-nested-message m)))
  (assert (not (pb:has-bb (pb:optional-nested-message m))))
  (assert (= (pb:bb (pb:optional-nested-message m)) 0))

  (assert (not (pb:has-optional-foreign-message m)))
  (assert (not (pb:has-c (pb:optional-foreign-message m))))
  (assert (= (pb:c (pb:optional-foreign-message m)) 0))

  (assert (not (pb:has-optional-import-message m)))
  (assert (not (pb:has-d (pb:optional-import-message m))))
  (assert (= (pb:d (pb:optional-import-message m)) 0))

  ;; repeated fields
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
          (let ((accessor (field-function "" field)))
            (assert (zerop (length (funcall accessor m))))))))

(defun modify-repeated-fields (m)
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
          (let ((accessor (field-function "" field))
                (v (third values)))
            (setf (aref (funcall accessor m) 1) v))))
  (setf (pb:a (aref (pb:repeatedgroup m) 1)) 517)
  (setf (pb::bb (aref (pb:repeated-nested-message m) 1)) 518)
  (setf (pb::c (aref (pb:repeated-foreign-message m) 1)) 519)
  (setf (pb::d (aref (pb:repeated-import-message m) 1)) 520))

(defun expect-repeated-fields-modified (m)
  (let ((field-info *repeated-field-info*))
    (loop for (field . values) in field-info do
          (let ((accessor (field-function "" field))
                (v0 (first values))
                (v1 (third values)))
            (assert (= (length (funcall accessor m)) 2))
            (assert (field-equal (aref (funcall accessor m) 0) v0))
            (assert (field-equal (aref (funcall accessor m) 1) v1)))))
  (let ((v (pb:repeatedgroup m)))
    (assert (= (length v) 2))
    (assert (= (pb:a (aref v 0)) 217))
    (assert (= (pb:a (aref v 1)) 517)))
  (let ((v (pb:repeated-nested-message m)))
    (assert (= (length v) 2))
    (assert (= (pb:bb (aref v 0)) 218))
    (assert (= (pb:bb (aref v 1)) 518)))
  (let ((v (pb:repeated-foreign-message m)))
    (assert (= (length v) 2))
    (assert (= (pb:c (aref v 0)) 219))
    (assert (= (pb:c (aref v 1)) 519)))
  (let ((v (pb:repeated-import-message m)))
    (assert (= (length v) 2))
    (assert (= (pb:d (aref v 0)) 220))
    (assert (= (pb:d (aref v 1)) 520))))

(defun test-modify-repeated-fields ()
  (let ((m (make-instance 'pb:testalltypes)))
    (expect-clear m)
    (set-all-fields m)
    (expect-all-fields-set m)
    (modify-repeated-fields m)
    (expect-repeated-fields-modified m)
    (pb:clear m)
    (expect-clear m)))

(defun test-serialize-and-merge ()
  (let ((m1 (make-instance 'pb:testalltypes))
        (m2 (make-instance 'pb:testalltypes))
        (m3 (make-instance 'pb:testalltypes)))
    (set-all-fields m1)
    (pb:clear m2)
    (pb:merge-from-message m2 m1)
    (let* ((size (pb:octet-size m1))
           (buffer (make-array size :element-type '(unsigned-byte 8))))
      (pb:serialize m1 buffer 0 size)
      (pb:merge-from-array m3 buffer 0 size))
    (expect-all-fields-set m2)
    (expect-all-fields-set m3)))

(defun test ()
  (test-parse-from-file)
  (test-parse-packed-from-file)
  (test-parse-helpers)
  (test-modify-repeated-fields)
  (test-serialize-and-merge)
  (print "PASS")
  (values))

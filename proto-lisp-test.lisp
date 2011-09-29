
;;;;    proto-lisp-test.lisp


;; Copyright 2008, Google Inc.
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

(in-package #:common-lisp-user)

(defpackage #:proto-lisp-test
  (:documentation "Test the Lisp implementation of protocol buffers.")
  (:use #:common-lisp
        #:com.google.base
        #:hu.dwim.stefil)
  (:export #:test-proto-lisp))

(in-package #:proto-lisp-test)
(declaim #.*optimize-default*)

(defsuite (test-proto-lisp :in root-suite) ()
  (run-child-tests))

(in-suite test-proto-lisp)

(defconst +pwd+ #.(make-pathname
                   :directory (pathname-directory
                               (or *compile-file-truename* *load-truename*))))

(defconst +golden-file-name+ (merge-pathnames "golden" +pwd+)
  "Pathname of a file containing correct protocol buffer data.")

(defconst +test-file-name+ (merge-pathnames "test-output-file" +pwd+)
  "Pathname of file to which we write protocol buffer data.")

(defmacro assert-string-equal ((field protobuf) string)
  (let* ((field-name (symbol-name field))
         (field-octets (intern (concatenate 'string field-name "-OCTETS")
                               "PROTOCOL-BUFFER")))
    `(progn (is (equalp (,field ,protobuf) ,string))
            (is (equalp (,field-octets ,protobuf) (string-to-utf8-octets ,string))))))

(deftest correctness-tests ()
  ;; Check that required strings are cleared by CLEAR.
  (let ((p (make-instance 'pb:TestProtocol)))
    (assert-string-equal (pb:zero p) "")
    (setf (pb:zero p) "x")
    (assert-string-equal (pb:zero p) "x")
    (pb:clear p)
    (assert-string-equal (pb:zero p) ""))

  ;; Check that optional strings are set to their default value by CLEAR.
  (let ((p (make-instance 'pb:TestProtocol)))
    (assert-string-equal (pb:optstring p) "opt")
    (setf (pb:optstring p) "x")
    (assert-string-equal (pb:optstring p) "x")
    (pb:clear p)
    (assert-string-equal (pb:optstring p) "opt")
    (setf (pb:optstring p) "x")
    (pb:clear p)
    (assert-string-equal (pb:optstring p) "opt")
    (setf (pb:optstring p) "x")
    (pb:clear-optstring p)
    (assert-string-equal (pb:optstring p) "opt"))
  (values))

(deftest test-pb-write ()
  (let ((p (make-instance 'pb:Test1Proto)))
    ;; verify enum values
    (is (= pb:+Test1Proto-EnumCode-FOO+ 0))
    (is (= pb:+Test1Proto-EnumCode-BAR+ 1))
    (is (= pb:+Test1Proto-EnumCode-BAZ+ 2))

    ;; default settings
    (is (= (pb:d-int32 p) 12))
    (assert-string-equal (pb:d-string p) "foo")
    (is (eq (pb:d-bool p) t))

    ;; test is-initialized
    (is (not (pb:is-initialized p)))
    (setf (pb:o-a p) 20)
    (is (pb:is-initialized p))

    ;; unrepeated things
    (setf (pb:u-int32 p) 20)
    (setf (pb:u-int64 p) -20)
    (setf (pb:u-uint64 p) 12345678900)
    (setf (pb:u-fixed32 p) 100)
    (setf (pb:u-fixed64 p) 12345678900)
    (setf (pb:u-bool p) t)
    (setf (pb:u-float p) 3.14159f0)
    (setf (pb:u-double p) 3.14159265d0)
    (setf (pb:u-string p) "foo")
    (setf (pb:u-vardata p) "bar")
    (setf (pb:foo (pb:u-msg p)) 12)

    ;; repeated things
    (vector-push-extend -20 (pb:r-int32 p))
    (vector-push-extend -30 (pb:r-int32 p))
    (vector-push-extend 20 (pb:r-int64 p))
    (vector-push-extend 30 (pb:r-int64 p))
    (vector-push-extend 12345678900 (pb:r-uint64 p))
    (vector-push-extend 98765432100 (pb:r-uint64 p))
    (vector-push-extend 12345 (pb:r-fixed32 p))
    (vector-push-extend 23456 (pb:r-fixed32 p))
    (vector-push-extend 12345678900 (pb:r-fixed64 p))
    (vector-push-extend 98765432100 (pb:r-fixed64 p))
    (vector-push-extend nil (pb:r-bool p))
    (vector-push-extend t (pb:r-bool p))
    (vector-push-extend 1.5f0 (pb:r-float p))
    (vector-push-extend -1.75f0 (pb:r-float p))
    (vector-push-extend 3.3d0 (pb:r-double p))
    (vector-push-extend -1.2d0 (pb:r-double p))
    (vector-push-extend (string-to-utf8-octets "foo") (pb:r-string p))
    (vector-push-extend (string-to-utf8-octets "bar") (pb:r-string p))
    (vector-push-extend (string-to-utf8-octets "ping") (pb:r-vardata p))
    (vector-push-extend (string-to-utf8-octets "pong") (pb:r-vardata p))

    (let ((x (make-instance 'pb:Test1Msg))
          (y (make-instance 'pb:Test1Msg)))
      (setf (pb:foo x) 12)
      (setf (pb:foo y) 13)
      (vector-push-extend x (pb:r-msg p))
      (vector-push-extend y (pb:r-msg p)))

    (let ((x (make-instance 'pb:Test1Proto-TestGroup1))
          (y (make-instance 'pb:Test1Proto-TestGroup2))
          (z (make-instance 'pb:Test1Proto-TestGroup2)))
      (setf (pb:a x) 80)
      (setf (pb:b y) 100)
      (setf (pb:b z) 130)
      (vector-push-extend x (pb:testgroup1 p))
      (vector-push-extend y (pb:testgroup2 p))
      (vector-push-extend z (pb:testgroup2 p)))

    ;; int32 tests
    (loop for x in (list (1- (ash 1 31)) (- (ash 1 31)) 1 0 -1)
          do (vector-push-extend x (pb:r-int32 p)))

    ;; int64 tests
    (loop for x in (list (1- (ash 1 63)) (- (ash 1 63)) 1 0 -1)
          do (vector-push-extend x (pb:r-int64 p)))

    ;; fixed32 tests
    (loop for x in (list #xffffffff (1- (ash 1 31)) 0 1)
          do (vector-push-extend x (pb:r-fixed32 p)))

    ;; fixed64 tests
    (loop for x in (list #xffffffffffffffff (1- (ash 1 63)) 0 1)
          do (vector-push-extend x (pb:r-fixed64 p)))

    ;; uint64 tests
    (loop for x in (list (1- (ash 1 64)) (1- (ash 1 63)) 0 1)
          do (vector-push-extend x (pb:r-uint64 p)))

    ;; write buffer to a file
    (let ((size (pb:octet-size p)))
      (let* ((output-buffer (make-octet-vector size))
             (end (pb:serialize p output-buffer 0 size)))
        (is (= end size))
        (with-open-file (output-stream +test-file-name+ :direction :output
                         :if-exists :supersede :element-type 'unsigned-byte)
          (write-sequence output-buffer output-stream)))

      ;; check against the golden data
      (with-open-file (golden-input +golden-file-name+ :direction :input
                       :element-type 'unsigned-byte)
        (is (= (file-length golden-input) size))
        (with-open-file (test-input +test-file-name+ :direction :input
                         :element-type 'unsigned-byte)
          (is (= (file-length test-input) size))
          (let ((golden-buffer (make-octet-vector size))
                (test-buffer (make-octet-vector size)))
            (read-sequence golden-buffer golden-input)
            (read-sequence test-buffer test-input)
            (is (equalp golden-buffer test-buffer))))))

    ;; clean up
    (delete-file +test-file-name+)))

(defun test-repeated (value golden)
  (let ((golden-size (length golden)))
    (is (= (length value) golden-size))
    (loop for v across value
          for g in golden
          ;; V and G are either NIL/T, numbers, or strings, actually simple
          ;; arrays of octets.
          do (cond ((and (member v '(t nil)) (member g '(t nil)))
                    (is (eq v g)))
                   ((and (numberp v) (numberp g)) (is (= v g)))
                   ((and (arrayp v) (arrayp g)) (is (equalp v g)))
                   (t (is (progn "type mismatch" nil)))))))

(deftest test-pb-read ()
  (let ((p (make-instance 'pb:Test1Proto)))
    (with-open-file (golden-input +golden-file-name+ :direction :input
                     :element-type 'unsigned-byte)
      (let* ((size (file-length golden-input))
             (buffer (make-octet-vector size)))
        (read-sequence buffer golden-input)
        (is (= (pb:merge-from-array p buffer 0 size) size))))

    ;; unrepeated things
    (is (pb:has-o-a p))
    (is (= (pb:o-a p) 20))
    (is (not (pb:has-o-b p)))
    (is (= (pb:u-int32 p) 20))
    (is (= (pb:u-int64 p) -20))
    (is (= (pb:u-uint64 p) 12345678900))
    (is (= (pb:u-fixed32 p) 100))
    (is (= (pb:u-fixed64 p) 12345678900))
    (is (eq (pb:u-bool p) t))
    (is (= (pb:u-float p) 3.14159f0))
    (is (= (pb:u-double p) 3.14159265d0))

    ;; Lisp implementation omits "has" function for embedded messages.
    ;;(is (has-u-msg p))
    (is (= (pb:foo (pb:u-msg p)) 12))

    ;; repeated things
    (test-repeated (pb:r-int32 p)
                   (list -20 -30 (1- (ash 1 31)) (- (ash 1 31)) 1 0 -1))
    (test-repeated (pb:r-int64 p)
                   (list 20 30 (1- (ash 1 63)) (- (ash 1 63)) 1 0 -1))
    (test-repeated (pb:r-uint64 p)
                   (list 12345678900 98765432100
                         (1- (ash 1 64)) (1- (ash 1 63))
                         0 1))
    (test-repeated (pb:r-fixed32 p)
                   (list 12345 23456 #xffffffff (1- (ash 1 31)) 0 1))
    (test-repeated (pb:r-fixed64 p)
                   (list 12345678900 98765432100 #xffffffffffffffff
                         (1- (ash 1 63)) 0 1))
    (test-repeated (pb:r-bool p) '(nil t))
    (test-repeated (pb:r-float p) '(1.5f0 -1.75f0))
    (test-repeated (pb:r-double p) '(3.3d0 -1.2d0))
    (test-repeated (pb:r-string p)
                   (list (string-to-utf8-octets "foo") (string-to-utf8-octets "bar")))
    (test-repeated (pb:r-vardata p)
                   (list (string-to-utf8-octets "ping") (string-to-utf8-octets "pong")))

    (is (= (length (pb:r-msg p)) 2))
    (is (= (pb:foo (aref (pb:r-msg p) 0)) 12))
    (is (= (pb:foo (aref (pb:r-msg p) 1)) 13))

    ;; groups
    (is (= (length (pb:testgroup1 p)) 1))
    (is (= (pb:a (aref (pb:testgroup1 p) 0)) 80))

    (is (= (length (pb:testgroup2 p)) 2))
    (is (= (pb:b (aref (pb:testgroup2 p) 0)) 100))
    (is (= (pb:b (aref (pb:testgroup2 p) 1)) 130))

    ;; default settings
    (is (= (pb:d-int32 p) 12))
    (assert-string-equal (pb:d-string p) "foo")
    (is (eq (pb:d-bool p) t))))

(defun parser-timing (iterations)
  (let ((src (make-instance 'pb:TimeProtocol)))
    (dotimes (i 1000)
      (let ((new (make-instance 'pb:TimeProtocol-G)))
        (setf (pb:v1 new) 100)
        (setf (pb:v2 new) 80)
        (vector-push-extend new (pb:g src))))

    (let* ((buffer (make-octet-vector 10000))
           ;; XXXXXXXXXX
           (size (pb:serialize src buffer 0 10000)))
      (time (dotimes (i iterations)
              (let ((msg (make-instance 'pb:TimeProtocol)))
                (pb:merge-from-array msg buffer 0 size)))))))

;; XXXXXXXXXXXXXXXXXXXX use parser-timing here

;; (defun output-timing ())
;; (defun test-copy-and-merge ())
;; (defun auto-tests ())

;; (defun test (&key
;;              (time-raw-parse nil)
;;              (time-auto-parse nil)
;;              (time-raw-output nil)
;;              (time-auto-output nil)
;;              (iterations 10000))
;;   (if (or time-raw-parse time-auto-parse)
;;       (parser-timing iterations)
;;       (if (or time-raw-output time-auto-output)
;;           (output-timing)
;;           (progn (correctness-tests)
;;                  (test-pb-write)
;;                  (test-pb-read)
;;                  (test-copy-and-merge)
;;                  ;; XXXX initialize random number generator ??
;;                  (auto-tests)
;;                  (print "PASS"))))
;;   (values))

;; XXXXXXXXXXXXXXXXXXXX add more test code here

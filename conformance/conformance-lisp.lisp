
;;;; Protocol buffer conformance test


(in-package #:common-lisp-user)

(defpackage #:conformance-test
 (:documentation "A conformance test for Google's Protocol Buffers.")
 (:use #:common-lisp
       #:com.google.base)
 (:export #:main))

(in-package #:conformance-test)

(defun parse-c-string (c-string)
  (let ((buffer (make-octet-vector 100)))
    (loop for i upfrom 0 below (length c-string)
          for out upfrom 0 below (length buffer)
          do (let ((octet
                     (if (char= #\\ (aref c-string i))
                         (prog1 (parse-integer c-string :start (1+ i) :end (+ i 4) :radix 8)
                           (incf i 3))
                         (char-int (aref c-string i)))))
               (setf (aref buffer out) octet)))
    buffer))

(defun doit (buffer)
  (let* ((message (make-instance 'protobuf-test-messages.proto2:test-all-types-proto2))
         (size (length buffer))
         (end (pb:merge-from-array message buffer 0 (length buffer))))
    (when (= end size) message)))

(defun describe-buffer (buffer start limit)
  (let ((index start))
    (loop while (< index limit) do
      (multiple-value-bind (field-number wire-type new-index)
          (wire-format::parse-tag buffer index limit)
        (setf index new-index)
        (format *error-output* "@~A field ~A" index field-number)
        (case wire-type
          (#.wire-format::+varint+
           (multiple-value-bind (value new-index)
               (varint:parse-uint64-carefully buffer index limit)
             (format *error-output* " (varint ~A)~%" value)
             (setf index new-index)))
          (#.wire-format::+fixed64+
           (format *error-output* " (fixed64)~%")
           (let ((new-index (+ index 8)))
             (declare (type vector-index new-index))
             (when (> new-index limit) (error 'data-exhausted))
             (setf index new-index)))
          (#.wire-format::+length-delimited+
           (format *error-output* " (length delimited)")
           (multiple-value-bind (size new-index)
               (varint:parse-uint32-carefully buffer index limit)
             (declare (type uint32 size)
                      (type vector-index new-index))
             (when (> (+ new-index size) limit) (error 'data-exhausted))
             (format *error-output* " len ~A~%" size)
             (setf index (+ new-index size))))
          (#.wire-format::+start-group+
           (format *error-output* " (group)")
           (loop
             (multiple-value-bind (element-field-number element-wire-type new-index)
                 (wire-format::parse-tag buffer index limit)
               (cond ((/= element-wire-type wire-format::+end-group+)
                      (setf index
                            (wire-format:skip-field element-field-number
                                                    element-wire-type
                                                    buffer
                                                    new-index
                                                    limit)))
                     ((= element-field-number field-number) (return new-index))
                     (t (error 'alignment))))))
          (#.wire-format::+fixed32+
           (format *error-output* " (fixed32)")
           (let ((new-index (+ index 4)))
             (declare (type vector-index new-index))
             (when (> new-index index) (error 'data-exhausted))
             (setf index new-index)))
          (t
           (error 'alignment)))))))

;; ========================================

(defun parsing-error (error-message)
  (let ((response (make-instance 'conformance:conformance-response)))
    (setf (conformance:parse-error response) (pb:string-field error-message))
    response))

(defun runtime-error (error-message)
  (let ((response (make-instance 'conformance:conformance-response)))
    (setf (conformance:runtime-error response) (pb:string-field error-message))
    response))

(defun serialize-error (error-message)
  (let ((response (make-instance 'conformance:conformance-response)))
    (setf (conformance:serialize-error response) (pb:string-field error-message))
    response))

(defun protobuf-response (message)
  (let* ((size (pb:octet-size message))
         (output-buffer (make-octet-vector size))
         (end (pb:serialize message output-buffer 0 size)))
    (if (/= end size)
        (serialize-error "serialize error")
        (let ((response (make-instance 'conformance:conformance-response)))
          (setf (conformance:protobuf-payload response) output-buffer)
          response))))

(defun failure-set ()
  (let ((failure-set (make-instance 'conformance:failure-set)))
    (with-open-file (failures "failure_list_lisp.txt")
      (loop for failed-test = (read-line failures nil nil)
            while failed-test
            do (vector-push-extend (pb:string-field failed-test)
                                   (conformance:failure failure-set))))
    (protobuf-response failure-set)))

(defun handle-request (request)
  (when (string= (pb:string-value (conformance:message-type request)) "conformance.FailureSet")
    (return-from handle-request (failure-set)))

  ;; XXXX no need to make message if JSON output is requested.
  (let ((message (if (string= (pb:string-value (conformance:message-type request))
                              "protobuf_test_messages.proto2.TestAllTypesProto2")
                     (make-instance 'protobuf-test-messages.proto2:test-all-types-proto2)
                     (make-instance 'protobuf-test-messages.proto3:test-all-types-proto3))))
    (cond ((conformance:has-protobuf-payload request)
           (let* ((buffer (conformance:protobuf-payload request))
                  (size (length buffer)))
             ;; (ignore-errors (describe-buffer buffer 0 size))
             ;; XXXX This sucks.  Redo exception class hierarchy.
             (let ((end (handler-case (pb:merge-from-array message buffer 0 (length buffer))
                          (wire-format:data-exhausted ()
                            (return-from handle-request
                                         (parsing-error "wire format data exhausted")))
                          (wire-format:value-out-of-range ()
                            (return-from handle-request
                                         (parsing-error "wire format value out of range")))
                          (wire-format:alignment ()
                            (return-from handle-request
                                         (parsing-error "wire format alignment")))
                          (varint:data-exhausted ()
                            (return-from handle-request
                                         (parsing-error "varint data exhausted")))
                          (varint:value-out-of-range ()
                            (return-from handle-request
                                         (parsing-error "varint value out of range")))
                          (varint:alignment ()
                            (return-from handle-request
                                         (parsing-error "varint alignment")))
                          )))
               (unless (= end size)
                 (return-from handle-request (parsing-error "parse error"))))))
          (t
           (return-from handle-request (runtime-error "unknown request payload type"))))

    (unless (conformance:has-requested-output-format request)
      (return-from handle-request (runtime-error "unknown request payload type")))

    (let ((output-format (conformance:requested-output-format request)))
      (case output-format
        (#.conformance:+wire-format-protobuf+ (protobuf-response message))
        (t (runtime-error "unknown output format"))))))

(defun main ()
  (let ((size-buffer (make-octet-vector 4)))
    (loop
      (when (/= (read-sequence size-buffer *standard-input*) 4)
        (uiop/image:quit))
      (let* ((input-size (nibbles:ub32ref/le size-buffer 0))
             (input-buffer (make-octet-vector input-size)))
        (when (/= (read-sequence input-buffer *standard-input*) input-size)
          (uiop/image:quit :code 1))
        (let ((request (make-instance 'conformance:conformance-request)))
          (unless (= (pb:merge-from-array request input-buffer 0 input-size) input-size)
            (error "problem unmarshalling request"))
          (let* ((result (handle-request request))
                 (result-size (pb:octet-size result))
                 (output-buffer (make-octet-vector result-size))
                 (end (pb:serialize result output-buffer 0 result-size)))
            (unless (= end result-size)
              (error "problem marshalling result"))
            (setf (nibbles:ub32ref/le size-buffer 0) result-size)
            (write-sequence size-buffer *standard-output*)
            (write-sequence output-buffer *standard-output*)
            (force-output)))))))

;;;; Portable floating point encoding and decoding.

;;;; This software was extracted from the SBCL Common Lisp implementation, which was derived from
;;;; the CMU Common Lisp system, which was written at Carnegie Mellon University and released into
;;;; the public domain.  The software in this file is in the public domain.

(in-package #:portable-float)

(declaim (ftype (function (single-float) (values (signed-byte 32) &optional)) single-float-bits))

(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
      (if (eql x 0.0f0)
          0
          #x-80000000)
      (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
	  (integer-decode-float x)
	(assert (plusp lisp-significand))
	(let* ((significand lisp-significand)
	       (exponent (+ lisp-exponent 23 127))
	       (unsigned-result
                 (if (plusp exponent)	; if not obviously denormalized
                     (do ()
                         (nil)
                       (cond (;; special termination case, denormalized
                              ;; float number
                              (zerop exponent)
                              ;; Denormalized numbers have exponent one
                              ;; greater than the exponent field.
                              (return (ash significand -1)))
                             (;; ordinary termination case
                              (>= significand (expt 2 23))
                              (assert (< 0 significand (expt 2 24)))
                              ;; Exponent 0 is reserved for
                              ;; denormalized numbers, and 255 is
                              ;; reserved for specials like NaN.
                              (assert (< 0 exponent 255))
                              (return (logior (ash exponent 23)
                                              (logand significand
                                                      (1- (ash 1 23))))))

                             (t
                              ;; Shift as necessary to set bit 24 of
                              ;; significand.
                              (setf significand (ash significand 1)
                                    exponent (1- exponent)))))
                     (do ()
                         ((zerop exponent)
                          ;; Denormalized numbers have exponent one
                          ;; greater than the exponent field.
                          (ash significand -1))
                       (unless (zerop (logand significand 1))
                         (warn "denormalized SINGLE-FLOAT-BITS ~S losing bits"
                               x))
                       (setf significand (ash significand -1)
                             exponent (1+ exponent))))))
	  (ecase lisp-sign
	    (1 unsigned-result)
	    (-1 (logior unsigned-result (- (expt 2 31)))))))))

(declaim (ftype (function (double-float) (values (signed-byte 64) &optional)) double-float-bits))

(defun double-float-bits (x)
  (declare (type double-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
      (if (eql x 0.0d0)
          0
          #x-8000000000000000)
      (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
	  (integer-decode-float x)
	(assert (plusp lisp-significand))
	(let* ((significand lisp-significand)
	       (exponent (+ lisp-exponent 52 1023))
	       (unsigned-result
                 (if (plusp exponent)	; if not obviously denormalized
                     (do ()
                         (nil)
                       (cond (;; special termination case, denormalized
                              ;; float number
                              (zerop exponent)
                              ;; Denormalized numbers have exponent one
                              ;; greater than the exponent field.
                              (return (ash significand -1)))
                             (;; ordinary termination case
                              (>= significand (expt 2 52))
                              (assert (< 0 significand (expt 2 53)))
                              ;; Exponent 0 is reserved for
                              ;; denormalized numbers, and 2047 is
                              ;; reserved for specials like NaN.
                              (assert (< 0 exponent 2047))
                              (return (logior (ash exponent 52)
                                              (logand significand
                                                      (1- (ash 1 52))))))
                             (t
                              ;; Shift as necessary to set bit 53 of
                              ;; significand.
                              (setf significand (ash significand 1)
                                    exponent (1- exponent)))))
                     (do ()
                         ((zerop exponent)
                          ;; Denormalized numbers have exponent one
                          ;; greater than the exponent field.
                          (ash significand -1))
                       (unless (zerop (logand significand 1))
                         (warn "denormalized DOUBLE-FLOAT-BITS ~S losing bits"
                               x))
                       (setf significand (ash significand -1)
                             exponent (1+ exponent))))))
	  (ecase lisp-sign
	    (1 unsigned-result)
	    (-1 (logior unsigned-result (- (expt 2 63)))))))))

(declaim (ftype (function ((signed-byte 32)) (values single-float &optional))
                make-single-float))

(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)         ; XXXX change if unsigned-byte argument
    (t (let* ((sign (ecase (ldb (byte 1 31) bits)
                      (0  1.0)
                      (1 -1.0)))
              (iexpt (ldb (byte 8 23) bits))
              (exponent (if (zerop iexpt) ; denormalized
                            -126
                            (- iexpt 127)))
              (mantissa (* (logior (ldb (byte 23 0) bits)
                                   (if (zerop iexpt)
                                       0
                                       (ash 1 23)))
                           (expt 0.5 23))))
         (* sign (expt 2.0 exponent) mantissa)))))

(declaim (ftype (function ((signed-byte 32) (unsigned-byte 32)) (values double-float &optional))
                make-double-float))

(defun make-double-float (high-bits low-bits)
  (declare (type (signed-byte 32) high-bits)
           (type (unsigned-byte 32) low-bits))
  (cond
    ;; IEEE float special cases
    ((and (zerop high-bits) (zerop low-bits)) 0.0d0)
    ;; XXXX change if unsigned-byte arguments
    ((and (= high-bits #x-80000000) (zerop low-bits)) -0.0d0)
    (t (let* ((bits (logior (ash high-bits 32) low-bits))
              (sign (ecase (ldb (byte 1 63) bits)
                      (0  1.0d0)
                      (1 -1.0d0)))
              (iexpt (ldb (byte 11 52) bits))
              (exponent (if (zerop iexpt) ; denormalized
                            -1022
                            (- iexpt 1023)))
              (mantissa (* (logior (ldb (byte 52 0) bits)
                                   (if (zerop iexpt)
                                       0
                                       (ash 1 52)))
                           (expt 0.5d0 52))))
         (* sign (expt 2.0d0 exponent) mantissa)))))

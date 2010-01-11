
;;;;    wire-format-test.lisp


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


(in-package #:wire-format-test)

(declaim #.optimize:+default+)


(defun test-zig-zag-encoding ()
  (flet ((verify (fun arg-results)
           (loop for (arg result) in arg-results
                 do (assert (= (funcall fun arg) result)))))
    (verify #'zig-zag-encode32
            `((0 0) (-1 1) (1 2) (-2 3)
              (#x3fffffff #x7ffffffe)
              (,(- #xc0000000 (ash 1 32)) #x7fffffff)
              (#x7fffffff #xfffffffe)
              (,(- #x80000000 (ash 1 32)) #xffffffff)))
    (verify #'zig-zag-decode32
            `((0 0) (1 -1) (2 1) (3 -2)
              (#x7ffffffe #x3fffffff)
              (#x7fffffff ,(- #xc0000000 (ash 1 32)))
              (#xfffffffe #x7fffffff)
              (#xffffffff ,(- #x80000000 (ash 1 32)))))
    (verify #'zig-zag-encode64
            `((0 0) (-1 1) (1 2) (-2 3)
              (#x000000003fffffff #x000000007ffffffe)
              (,(- #xffffffffc0000000 (ash 1 64)) #x000000007fffffff)
              (#x000000007fffffff #x00000000fffffffe)
              (,(- #xffffffff80000000 (ash 1 64)) #x00000000ffffffff)
              (#x7fffffffffffffff #xfffffffffffffffe)
              (,(- #x8000000000000000 (ash 1 64)) #xffffffffffffffff)))
    (verify #'zig-zag-decode64
            `((0 0) (1 -1) (2 1) (3 -2)
              (#x000000007ffffffe #x000000003fffffff)
              (#x000000007fffffff ,(- #xffffffffc0000000 (ash 1 64)))
              (#x00000000fffffffe #x000000007fffffff)
              (#x00000000ffffffff ,(- #xffffffff80000000 (ash 1 64)))
              (#xfffffffffffffffe #x7fffffffffffffff)
              (#xffffffffffffffff ,(- #x8000000000000000 (ash 1 64))))))

  ;; Some easier-to-verify round-trip tests.  The inputs (other than 0, 1, -1)
  ;; were chosen semi-randomly via keyboard bashing.
  (flet ((round-trip32 (n)
           (assert (= n (zig-zag-decode32 (zig-zag-encode32 n)))))
         (round-trip64 (n)
           (assert (= n (zig-zag-decode64 (zig-zag-encode64 n))))))
    (dolist (n '(0 1 -1 14927 -3612))
      (round-trip32 n))
    (dolist (n '(0 1 -1 14927 -3612 856912304801416 -75123905439571256))
      (round-trip64 n)))
  (values))

(defun test ()
  (test-zig-zag-encoding)
  (print "PASS")
  (values))

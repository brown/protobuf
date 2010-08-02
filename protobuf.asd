
;;;;    protobuf.asd


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


(cl:in-package #:common-lisp-user)

(defpackage #:protobuf-system
  (:documentation "System definitions for protocol buffer code.")
  (:use #:common-lisp #:asdf)
  (:export #:proto-file
           #:*protobuf-compiler*))

(in-package #:protobuf-system)


;; Pathname of Google's protocol buffer compiler.  You must replace this
;; pathname with whatever is appropriate for your system by setting
;; PROTOBUF-ASD:*PROTOC* before this file is loaded.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*protoc*)
    (defparameter *protobuf-compiler* #p"/home/brown/toe/protobuf/google-protobuf/src/protoc"
      "Pathname of the protocol buffer compiler.")))


;;; Teach ASDF how to convert protocol buffer definition files into Lisp.


(defclass proto-file (cl-source-file)
  ((relative-proto-pathname
    :initarg :proto-pathname
    :initform nil
    :reader proto-pathname
   :documentation "Relative pathname that specifies a protobuf .proto file")
   (search-path
    :initform ()
    :initarg :proto-search-path
    :reader search-path
    :documentation "List containing directories in which the protocol buffer
compiler should search for imported protobuf files.  Non-absolute pathnames
are treated as relative to the protobuf source directory."))
  (:documentation "A protocol buffer definition file."))

(defclass proto-to-lisp (operation)
  ()
  (:documentation "An ASDF operation that compiles a file containing protocol
buffer definitions into a Lisp source code."))

(defmethod component-depends-on ((operation compile-op) (component proto-file))
  "Compiling a protocol buffer file depends on generating Lisp source code
for the protobuf, but also on loading package definitions and in-line
function definitions that the machine-generated protobuf Lisp code uses."
  `(;;(load-op "protocol-buffer" "varint" "wire-format")
    (proto-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defmethod component-depends-on ((operation load-op) (component proto-file))
  "Loading a protocol buffer file depends on generating Lisp source code
for the protobuf, but also on loading package definitions and in-line
function definitions that the machine-generated protobuf Lisp code uses."
  `(;;(load-op "protocol-buffer" "varint" "wire-format")
    (proto-to-lisp ,(component-name component))
    ,@(call-next-method)))

(defun proto-input (proto-file)
  "Return the pathname of the protocol buffer definition file that must be
translated into Lisp source code for this PROTO-FILE component."
  (if (proto-pathname proto-file)
      ;; Path of the protobuf file was specified with :PROTO-PATHNAME.
      (merge-pathnames
       (make-pathname :type "proto")
       (merge-pathnames (pathname (proto-pathname proto-file))
                        (asdf::component-parent-pathname proto-file)))
      ;; No :PROTO-PATHNAME was specified, so the path of the protobuf
      ;; defaults to that of the Lisp file, but with a ".proto" suffix.
      (let ((lisp-pathname (component-pathname proto-file)))
        (merge-pathnames (make-pathname :type "proto") lisp-pathname))))

(defmethod input-files ((operation proto-to-lisp) (component proto-file))
  (list *protobuf-compiler* (proto-input component)))

(defmethod output-files ((operation proto-to-lisp) (component proto-file))
  (values (list (component-pathname component))
          nil))                         ; allow around methods to translate

(defun resolve-relative-pathname (path parent-path)
  "When PATH doesn't have an absolute directory component, treat it as
relative to PARENT-PATH."
  (let* ((pathname (pathname path))
         (directory (pathname-directory pathname)))
    (if (and (list directory) (eq (car directory) :absolute))
        pathname
        (let ((resolved-path (merge-pathnames pathname parent-path)))
          (make-pathname :directory (pathname-directory resolved-path)
                         :name nil
                         :type nil
                         :defaults resolved-path)))))

(defun resolve-search-path (proto-file)
  (let ((search-path (search-path proto-file)))
    (let ((parent-path (component-pathname (component-parent proto-file))))
      (mapcar (lambda (path)
                (resolve-relative-pathname path parent-path))
              search-path))))

(defmethod perform ((operation proto-to-lisp) (component proto-file))
  (let* ((source-file (proto-input component))
         ;; Around methods on output-file may globally redirect output
         ;; products, so we must call that method instead of executing
         ;; (component-pathname component).
         (output-file (first (output-files operation component)))
         (search-path (cons (directory-namestring source-file) (resolve-search-path component)))
         (status (run-shell-command "~A --proto_path=~{~A~^:~} --lisp_out=~A ~A"
                                    (namestring *protobuf-compiler*)
                                    search-path
                                    (directory-namestring output-file)
                                    (namestring source-file))))
    (unless (zerop status)
      (error 'compile-failed :component component :operation operation))))

(defmethod asdf::component-self-dependencies ((op load-op) (c proto-file))
  "Remove PROTO-TO-LISP operations from self dependencies.  Otherwise, the
.lisp output files of PROTO-TO-LISP are considered to be input files for
LOAD-OP, which means ASDF loads both the .lisp file and the .fasl file."
  (remove-if (lambda (x)
               (eq (car x) 'proto-to-lisp))
             (call-next-method)))


;;; Protocol buffer support code.


(defsystem protobuf
  :name "Protocol Buffer"
  :description "Protocol buffer code"
  :long-description "A Common Lisp implementation of Google's protocol
buffer support libraries."
  :version "0.4"
  :author "Robert Brown"
  :licence "See file COPYING and the copyright messages in individual files."
  ;; After loading the system, announce its availability.
  :perform (load-op :after (operation component)
             (pushnew :protobuf cl:*features*)
             (provide 'protobuf))
  :depends-on (#-(or allegro clisp sbcl) :trivial-utf-8)
  :components
  ((:static-file "COPYING")
   (:static-file "README")
   (:static-file "TODO")
   (:cl-source-file "package")
   #-(or abcl allegro cmu sbcl)
   (:module "sysdep"
    :pathname ""           ; this module's files are not in a subdirectory
    :depends-on ("package")
    :components ((:cl-source-file "portable-float")))
   (:cl-source-file "optimize" :depends-on ("package"))
   (:cl-source-file "base" :depends-on ("package" "optimize"))
   (:cl-source-file "varint"  :depends-on ("package" "optimize" "base"))
   (:cl-source-file "protocol-buffer" :depends-on ("package"))
   ;; The varint dependency is needed because some varint functions are
   ;; declared in line, and so must be loaded before wire-format is compiled.
   (:cl-source-file "wire-format"
    :depends-on ("package" "base" "optimize" "varint"
                 #-(or abcl allegro cmu sbcl) "sysdep"))
   ))


(in-package #:cl-user)

(defun load-quicklisp ()
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (if (probe-file quicklisp-init)
        (load quicklisp-init)
        (error "cannot find Quicklist setup file ~S" quicklisp-init))))

(load-quicklisp)
(asdf:load-system 'protobuf-conformance)
(sb-ext:save-lisp-and-die "conformance-sbcl" :executable t :toplevel #'conformance-test:main)

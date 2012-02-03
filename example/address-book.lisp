
;;;;    address-book-example.lisp


(in-package #:address-book)

(defparameter *address-file-name* #p"ADDRESS_BOOK_FILE")

(defun read-address-book ()
  (let ((address-book (make-instance 'address-book)))
    (when (probe-file *address-file-name*)
      (with-open-file (input *address-file-name*
                       :direction :input :element-type 'unsigned-byte)
        (let* ((size (file-length input))
               (buffer (make-array size :element-type '(unsigned-byte 8))))
          (read-sequence buffer input)
          (pb:merge-from-array address-book buffer 0 size))))
    address-book))

(defun write-address-book (address-book)
  (let* ((size (pb:octet-size address-book))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (pb:serialize address-book buffer 0 size)
    (with-open-file (output *address-file-name*
                     :direction :output :if-exists :supersede
                     :element-type 'unsigned-byte) ; XXXX
      (write-sequence buffer output)))
  (values))

(defun add-person (&key id name email-address phone-numbers)
  (let ((address-book (read-address-book))
        (person (make-instance 'person)))
    (setf (id person) id)
    (setf (name person) (pb:string-field name))
    (when email-address
      (setf (email person) (pb:string-field email-address)))
    (loop for (type . number) in phone-numbers do
      (let ((phone-number (make-instance 'person-phone-number))
            (type (ecase type
                    (mobile +person-phone-type-mobile+)
                    (home +person-phone-type-home+)
                    (work +person-phone-type-work+))))
        (setf (number phone-number) (pb:string-field number))
        (setf (type phone-number) type)
        (vector-push-extend phone-number (phone person))))
    (vector-push-extend person (person address-book))
    (write-address-book address-book))
  (values))

(defun list-people ()
  (let ((address-book (read-address-book)))
    (loop for person across (person address-book) do
      (format t "Person ID: ~A~%" (id person))
      (format t "  Name: ~A~%" (pb:string-value (name person)))
      (when (has-email person)
        (format t "  E-mail address: ~A~%" (pb:string-value (email person))))
      (loop for phone-number across (phone person) do
        (let ((location
                (ecase (type phone-number)
                  (#.+person-phone-type-mobile+ "Mobile")
                  (#.+person-phone-type-home+ "Home")
                  (#.+person-phone-type-work+ "Work"))))
          (format t "  ~A phone #: ~A~%"
                  location (pb:string-value (number phone-number)))))))
  (values))

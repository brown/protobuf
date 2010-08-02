
;;;;    address-book-example.lisp


(in-package #:address-book)

(defparameter *address-file-name* #p"ADDRESS_BOOK_FILE")

(defun read-address-book ()
  (let ((address-book (make-instance 'pb:addressbook)))
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
        (person (make-instance 'pb:person)))
    (setf (pb:id person) id)
    (setf (pb:name person) name)
    (when email-address
      (setf (pb:email person) email-address))
    (loop for (type . number) in phone-numbers do
          (let ((phone-number (make-instance 'pb:person-phonenumber))
                (type (ecase type
                        (mobile pb:+person-phonetype-mobile+)
                        (home pb:+person-phonetype-home+)
                        (work pb:+person-phonetype-work+))))
            (setf (pb:number phone-number) number)
            (setf (pb:type phone-number) type)
            (vector-push-extend phone-number (pb:phone person))))
    (vector-push-extend person (pb:person address-book))
    (write-address-book address-book))
  (values))

(defun list-people ()
  (let ((address-book (read-address-book)))
    (loop for person across (pb:person address-book) do
          (format t "Person ID: ~A~%" (pb:id person))
          (format t "  Name: ~A~%" (pb:name person))
          (when (pb:has-email person)
            (format t "  E-mail address: ~A~%" (pb:email person)))
          (loop for phone-number across (pb:phone person) do
                (let ((location
                       (ecase (pb:type phone-number)
                         (#.pb:+person-phonetype-mobile+ "Mobile")
                         (#.pb:+person-phonetype-home+ "Home")
                         (#.pb:+person-phonetype-work+ "Work"))))
                  (format t "  ~A phone #: ~A~%"
                          location (pb:number phone-number))))))
  (values))

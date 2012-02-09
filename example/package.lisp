
;;;;    package.lisp


(in-package #:common-lisp-user)

(defpackage #:address-book
  (:use #:common-lisp)
  (:import-from #:tutorial
                #:+person-phone-type-home+
                #:+person-phone-type-mobile+
                #:+person-phone-type-work+
                #:address-book
                #:email
                #:has-email
                #:id
                #:name
                #:person
                #:person-phone-number
                #:phone)
  ;; The "number" and "type" field names conflict with Common Lisp symbols.
  (:shadowing-import-from #:tutorial
                          #:number
                          #:type)
  (:export #:add-person
           #:list-people))

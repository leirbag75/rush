
(defpackage :rush
  (:use :cl)
  (:import-from :closer-mop
                #:class-slots
                #:slot-definition-name
                #:subclassp)
  (:shadow #:speed))

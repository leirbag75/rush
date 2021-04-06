
(in-package :rush)

(defgeneric name (object)
  (:method (object)
    (format nil "~A" object)))

(defclass event ()
  ())

(defgeneric default-message (event)
  (:method ((event event))
    ""))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "~A" (default-message event))))

(defgeneric events-match-p (event1 event2)
  (:method (event1 event2)
    (eql event1 event2)))

(defclass damage-infliction (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount))
  (:default-initargs :target nil :amount 0))

(defmethod events-match-p ((x damage-infliction) (y damage-infliction))
  (and (eql (target x) (target y))
       (eql (amount x) (amount y))))

(defmethod default-message ((event damage-infliction))
  (format nil
          "~A took ~A damage."
          (name (target event))
          (amount event)))

(defclass death (event)
  ((target :reader target
           :initarg :target))
  (:default-initargs :target nil))

(defmethod default-message ((event death))
  (format nil
          "~A died."
          (name (target event))))

(defmethod events-match-p ((x death) (y death))
  (eql (target x) (target y)))

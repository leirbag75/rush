
(in-package :rush)

(defclass event ()
  ())

(defgeneric events-match-p (event1 event2)
  (:method ((event1 event) (event2 event))
    nil))

(defclass damage-infliction (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount)))

(defmethod events-match-p ((x damage-infliction) (y damage-infliction))
  (and (eql (target x) (target y))
       (eql (amount x) (amount y))))

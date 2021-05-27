
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

(defmethod events-match-p ((x event) (y event))
  (let ((class-x (class-of x))
        (class-y (class-of y)))
    (every (lambda (slot)
             (equal (slot-value x (slot-definition-name slot))
                    (slot-value y (slot-definition-name slot))))
           (class-slots (cond
                          ((subclassp class-x class-y)
                           class-y)
                          ((subclassp class-y class-x)
                           class-x)
                          (t
                           (return-from events-match-p nil)))))))

(defclass damage-infliction (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount))
  (:default-initargs :target nil :amount 0))

(defmethod default-message ((event damage-infliction))
  (format nil
          "~A took ~A damage."
          (name (target event))
          (amount event)))

(defclass damage-heal (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount)))

(defmethod default-message ((event damage-heal))
  (format nil
          "~A recovered ~A damage."
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

(defclass move-use (event)
  ((move :reader move
         :initarg :move)
   (user :reader user
         :initarg :user)))

(defmethod default-message ((event move-use))
  (format nil
          "~A used ~A."
          (name (user event))
          (name (move event))))

(defclass momentum-gain (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount)))

(defmethod default-message ((event momentum-gain))
  (format nil
          "~A gained ~A momentum."
          (name (target event))
          (amount event)))

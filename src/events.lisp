
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

(defclass negated-event (event)
  ((event :reader event
          :initarg :event)))

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
         :initarg :user)
   (targets :reader targets
            :initarg :targets))
  (:default-initargs :targets '()))

(defmethod default-message ((event move-use))
  (format nil
          "~A used ~A~@[ on ~{~A~#[~; and ~;, ~A, and ~A~:;, ~]~}~]."
          (name (user event))
          (name (move event))
          (mapcar #'name (targets event))))

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

(defclass enter-rush-mode (event)
  ((target :reader target
           :initarg :target)))

(defmethod default-message ((event enter-rush-mode))
  (format nil
          "~A entered rush mode."
          (name (target event))))

(defclass exit-rush-mode (event)
  ((target :reader target
           :initarg :target)))

(defmethod default-message ((event exit-rush-mode))
  (format nil
          "~A exited rush mode."
          (name (target event))))

(defclass action-gain (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount))
  (:default-initargs :amount 1))

(defmethod default-message ((event action-gain))
  (format nil
          "~A gained ~D action~:P."
          (name (target event))
          (amount event)))

(defclass action-loss (event)
  ((target :reader target
           :initarg :target)
   (amount :reader amount
           :initarg :amount))
  (:default-initargs :amount 1))

(defmethod default-message ((event action-loss))
  (format nil
          "~A lost ~D action~:P."
          (name (target event))
          (amount event)))

(defclass momentum-reset (event)
  ((target :reader target
           :initarg :target)))

(defmethod default-message ((event momentum-reset))
  (format nil
          "~A lost momentum."
          (name (target event))))

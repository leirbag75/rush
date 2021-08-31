
(in-package :rush)

(defgeneric name (object)
  (:method (object)
    (format nil "~A" object)))

(defclass event ()
  ())

(defmethod default-message ((event event))
  "")

(defmethod event-validp and ((event event) battle)
  t)

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

(defclass live-target ()
  ((target :reader target
           :initarg :target)))

(defmethod event-validp and ((event live-target) battle)
  (not (combatant-deadp battle (target event))))

(defclass mobile-user ()
  ((user :reader user
         :initarg :user)))

(defmethod event-validp and ((event mobile-user) battle)
  (not (combatant-incapacitatedp battle (user event))))

(defclass damage-infliction (live-target event)
  ((amount :reader amount
           :initarg :amount)))

(defmethod default-message ((event damage-infliction))
  (format nil
          "~A took ~A damage."
          (name (target event))
          (amount event)))

(defclass damage-heal (live-target event)
  ((amount :reader amount
           :initarg :amount)))

(defmethod default-message ((event damage-heal))
  (format nil
          "~A recovered ~A damage."
          (name (target event))
          (amount event)))

(defclass death (event)
  ((target :reader target
           :initarg :target)))

(defmethod default-message ((event death))
  (format nil
          "~A died."
          (name (target event))))

(defclass move-use (mobile-user event)
  ((targets :reader targets
            :initarg :targets)
   (move :reader move
         :initarg :move))
  (:default-initargs :targets '()))

(defmethod default-message ((event move-use))
  (format nil
          "~A used ~A~@[ on ~{~A~#[~; and ~;, ~A, and ~A~:;, ~]~}~]."
          (name (user event))
          (name (move event))
          (mapcar #'name (targets event))))

(defclass momentum-gain (live-target event)
  ((amount :reader amount
           :initarg :amount)))

(defmethod default-message ((event momentum-gain))
  (format nil
          "~A gained ~A momentum."
          (name (target event))
          (amount event)))

(defclass enter-rush-mode (live-target event)
  ())

(defmethod default-message ((event enter-rush-mode))
  (format nil
          "~A entered rush mode."
          (name (target event))))

(defclass exit-rush-mode (live-target event)
  ())

(defmethod default-message ((event exit-rush-mode))
  (format nil
          "~A exited rush mode."
          (name (target event))))

(defclass action-gain (live-target event)
  ((amount :reader amount
           :initarg :amount))
  (:default-initargs :amount 1))

(defmethod default-message ((event action-gain))
  (format nil
          "~A gained ~D action~:P."
          (name (target event))
          (amount event)))

(defclass action-loss (live-target event)
  ((amount :reader amount
           :initarg :amount))
  (:default-initargs :amount 1))

(defmethod default-message ((event action-loss))
  (format nil
          "~A lost ~D action~:P."
          (name (target event))
          (amount event)))

(defclass momentum-reset (live-target event)
  ())

(defmethod default-message ((event momentum-reset))
  (format nil
          "~A lost momentum."
          (name (target event))))


(in-package :rush)

(defclass event-accumulator ()
  ((events :accessor events
           :initform '())))

(defmethod next-events ((event-accumulator event-accumulator))
  (prog1 (reverse (events event-accumulator))
    (setf (events event-accumulator) '())))

(defmethod add-event ((event-accumulator event-accumulator) event)
  (push event (events event-accumulator)))

(defmethod negate-event ((event-accumulator event-accumulator) event)
  (setf (events event-accumulator)
        (substitute (make-instance 'negated-event :event event)
                    event
                    (events event-accumulator))))

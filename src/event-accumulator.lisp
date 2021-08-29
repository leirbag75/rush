
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

(defmethod preempt-event ((event-accumulator event-accumulator)
                          preempted-event
                          preempting-event)
  (setf (events event-accumulator)
        ;; Sorry to write something like this, but this was the most
        ;; obvious way for me to do it... (Basically, it's using the
        ;; list monad)
        (reduce #'nconc
                (events event-accumulator)
                :from-end t
                :key (lambda (event)
                       ;; Since the event list is reversed before
                       ;; being returned, the preempted-event comes
                       ;; *first*
                       (if (eql event preempted-event)
                           (list preempted-event preempting-event)
                           (list event))))))

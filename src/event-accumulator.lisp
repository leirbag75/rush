
(in-package :rush)

(defclass event-accumulator ()
  ((queue-front :accessor queue-front
                :initform '())
   (queue-back :accessor queue-back
               :initform '())))

(defun slink (event-accumulator)
  (setf (queue-front event-accumulator)
        (append (queue-front event-accumulator)
                (reverse (queue-back event-accumulator))))
  (setf (queue-back event-accumulator) '()))

(defmethod next-events ((event-accumulator event-accumulator))
  (slink event-accumulator)
  (prog1 (queue-front event-accumulator)
    (setf (queue-front event-accumulator) '())))

(defmethod add-event ((event-accumulator event-accumulator) event)
  (push event (queue-back event-accumulator)))

(defmethod negate-event ((event-accumulator event-accumulator) event)
  (slink event-accumulator)
  (setf (queue-front event-accumulator)
        (substitute (make-instance 'negated-event :event event)
                    event
                    (queue-front event-accumulator))))

(defun unnegated (event)
  (typecase event
    (negated-event (event event))
    (otherwise event)))

(defmethod preempt-event ((event-accumulator event-accumulator)
                          preempted-event
                          preempting-event)
  (slink event-accumulator)
  (setf (queue-front event-accumulator)
        ;; Sorry to write something like this, but this was the most
        ;; obvious way for me to do it... (Basically, it's using the
        ;; list monad)
        (reduce #'nconc
                (queue-front event-accumulator)
                :from-end t
                :key (lambda (event)
                       ;; Since the event list is reversed before
                       ;; being returned, the preempted-event comes
                       ;; *first*
                       (if (eql (unnegated event) preempted-event)
                           (list preempting-event event)
                           (list event))))))

(defmethod postempt-event ((event-accumulator event-accumulator)
                           postempted-event postempting-event)
  (slink event-accumulator)
  (setf (queue-front event-accumulator)
        ;; See comments above
        (reduce #'nconc
                (queue-front event-accumulator)
                :from-end t
                :key (lambda (event)
                       (if (eql (unnegated event) postempted-event)
                           (list event postempting-event)
                           (list event))))))

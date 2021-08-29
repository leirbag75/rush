
(in-package :rush)

(defgeneric next-events (battle))

(defgeneric add-event (accumulator event))

(defgeneric negate-event (accumulator event))

(defgeneric preempt-event (accumulator preempted-event preempting-event))

(defgeneric postempt-event (accumulator postempted-event postempting-event))

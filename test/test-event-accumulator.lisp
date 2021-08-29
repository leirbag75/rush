
(in-package :rush)

(deftest starts-empty (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (assert-events-match event-accumulator)))

(deftest accumulates-events (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (add-event event-accumulator 2)
    (assert-events-match event-accumulator
                         1 2)))

(deftest discards-events (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (next-events event-accumulator)
    (assert-events-match event-accumulator)))

(deftest replaces-event-with-negated-event (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (negate-event event-accumulator 1)
    (assert-events-match event-accumulator
                         (make-instance 'negated-event
                                        :event 1))))

(deftest adds-preempting-event-before-preempted (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (add-event event-accumulator 3)
    (preempt-event event-accumulator 3 2)
    (assert-events-match event-accumulator
                         1 2 3)))

(deftest adds-postempting-event-after-postempted (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (add-event event-accumulator 2)
    (add-event event-accumulator 4)
    (postempt-event event-accumulator 2 3)
    (assert-events-match event-accumulator
                         1 2 3 4)))

(deftest adds-event-even-if-negated (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (negate-event event-accumulator 1)
    (preempt-event event-accumulator 1 0)
    (postempt-event event-accumulator 1 2)
    (assert-events-match event-accumulator
                         0 (make-instance 'negated-event :event 1) 2)))

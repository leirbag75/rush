
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

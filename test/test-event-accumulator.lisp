
(in-package :rush)

(defun all-events-match-p (expected actual)
  (lists-match-p #'events-match-p expected actual))

(defun assert-events-match (expected actual)
  (unless (all-events-match-p expected actual)
    (error 'objects-dont-match
           :expected expected
           :actual actual)))

(deftest starts-empty (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (assert-events-match '()
                         (next-events event-accumulator))))

(deftest accumulates-events (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (add-event event-accumulator 2)
    (assert-events-match '(1 2)
                         (next-events event-accumulator))))

(deftest discards-events (test-event-accumulator)
  (let ((event-accumulator (make-instance 'event-accumulator)))
    (add-event event-accumulator 1)
    (next-events event-accumulator)
    (assert-events-match '()
                         (next-events event-accumulator))))

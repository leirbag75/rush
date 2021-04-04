
(in-package :rush)

(defgeneric events-match-p (event1 event2)
  (:method ((event1 event) (event2 event))
    nil))

(defun all-events-match-p (expected actual)
  (lists-match-p #'events-match-p expected actual))

(defun assert-events-match (expected actual)
  (unless (all-events-match-p expected actual)
    (error 'objects-dont-match
           :expected expected
           :actual actual)))

(defclass mock-combatant ()
  ())

(deftest battle-base-case (test-battle)
  (let ((battle (make-instance 'battle)))
    (assert-events-match '()
                         (next-events battle))))

(deftest battle-single-event (test-battle)
  (let* ((combatant (make-instance 'mock-combatant))
         (battle (make-instance 'battle
                                :combatants (list combatant))))
    (inflict-damage battle combatant 10)
    (assert-events-match (list (make-instance 'damage-infliction
                                              :target combatant
                                              :amount 10))
                         (next-events battle))))

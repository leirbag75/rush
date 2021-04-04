
(in-package :rush)

(defun all-events-match-p (expected actual)
  (lists-match-p #'events-match-p expected actual))

(defun assert-events-match (expected actual)
  (unless (all-events-match-p expected actual)
    (error 'objects-dont-match
           :expected expected
           :actual actual)))

(defvar *mock-combatant-number* 0)

(defclass mock-combatant ()
  ((name :reader name
         :initform (format nil "Mock ~A" (incf *mock-combatant-number*)))))

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

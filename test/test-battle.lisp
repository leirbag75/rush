
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

(defun make-battle-with-combatants (&rest combatants)
  (make-instance 'battle :combatants combatants))

(defparameter *weak-damage* 10)

(defun inflict-weak-damage (battle combatant)
  (inflict-damage battle combatant *weak-damage*))

(defun make-weak-damage-event (combatant)
  (make-instance 'damage-infliction
                 :target combatant
                 :amount *weak-damage*))

(defun make-mock-combatant ()
  (make-instance 'mock-combatant))

(defun resulting-events (battle)
  (next-events battle))

(deftest battle-base-case (test-battle)
  (let ((battle (make-battle-with-combatants)))
    (assert-events-match '()
                         (resulting-events battle))))

(deftest battle-single-event (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (inflict-weak-damage battle combatant)
    (assert-events-match (list (make-weak-damage-event combatant))
                         (resulting-events battle))))

(deftest battle-discard-events (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (inflict-weak-damage battle combatant)
    (resulting-events battle)
    (assert-events-match '()
                         (resulting-events battle))))

(deftest battle-multiple-events (test-battle)
  (let* ((combatant (make-mock-combatant))
         (combatant2 (make-mock-combatant))
         (battle (make-battle-with-combatants combatant combatant2)))
    (inflict-weak-damage battle combatant)
    (inflict-weak-damage battle combatant2)
    (assert-events-match (list (make-weak-damage-event combatant)
                               (make-weak-damage-event combatant2))
                         (resulting-events battle))))

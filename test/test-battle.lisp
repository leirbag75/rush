
(in-package :rush)

(defvar *mock-combatant-number* 0)

(defparameter *mock-combatant-max-hp* 200)

(defclass mock-combatant ()
  ((name :reader name
         :initform (format nil "Mock ~A" (incf *mock-combatant-number*)))))

(defmethod max-hp ((combatant mock-combatant))
  *mock-combatant-max-hp*)

(defun make-battle-with-combatants (&rest combatants)
  (make-instance 'battle :combatants combatants))

(defun half-kill (battle combatant)
  (inflict-damage battle combatant (floor *mock-combatant-max-hp* 2)))

(defun make-half-kill-event (combatant)
  (make-instance 'damage-infliction
                 :target combatant
                 :amount (floor *mock-combatant-max-hp* 2)))

(defun make-death-event (combatant)
  (make-instance 'death :target combatant))

(defun make-mock-combatant ()
  (make-instance 'mock-combatant))

(defun resulting-events (battle)
  (next-events battle))

(deftest battle-death (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (half-kill battle combatant)
    (half-kill battle combatant)
    (assert-events-match (list (make-half-kill-event combatant)
                               (make-half-kill-event combatant)
                               (make-death-event combatant))
                         (resulting-events battle))))

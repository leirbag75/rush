
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

(defun make-mock-combatant ()
  (make-instance 'mock-combatant))

(deftest subtracts-hp (test-battle)
  (let* ((combatant (make-mock-combatant))
         (remaining-hp (- *mock-combatant-max-hp* 10))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant 10)
    (unless (eql (remaining-hp battle combatant) remaining-hp)
      (error 'objects-dont-match
             :expected remaining-hp
             :actual (remaining-hp battle combatant)))))

(deftest adds-damage-infliction-event (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant 10)
    (assert-events-match (next-events battle)
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount 10))))

(deftest battle-death (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant *mock-combatant-max-hp*)
    (assert-events-match (next-events battle)
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount *mock-combatant-max-hp*)
                         (make-instance 'death :target combatant))))

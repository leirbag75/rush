
(in-package :rush)

(defclass battle ()
  ((combatants :reader combatants
               :initarg :combatants)
   (event-accumulator :reader event-accumulator
                      :initform (make-instance 'event-accumulator))
   (remaining-hp-table :reader remaining-hp-table
                       :initform (make-hash-table))))

(defmethod next-events ((battle battle))
  (next-events (event-accumulator battle)))

(defmethod remaining-hp ((battle battle) combatant)
  (or (gethash combatant (remaining-hp-table battle))
      (setf (remaining-hp battle combatant)
            (max-hp combatant))))

(defun (setf remaining-hp) (amount battle combatant)
  (setf (gethash combatant (remaining-hp-table battle))
        amount))

(defmethod add-event ((battle battle) event)
  (add-event (event-accumulator battle) event))

(defmethod inflict-damage ((battle battle) combatant amount)
  (add-event battle
             (make-instance 'damage-infliction
                            :target combatant
                            :amount amount))
  (let ((remaining-hp (decf (remaining-hp battle combatant) amount)))
    (when (<= remaining-hp 0)
      (add-event battle
                 (make-instance 'death
                                :target combatant)))))

(defmethod input-moves ((battle battle) combatant moves)
  (dolist (move moves)
    (add-event battle (make-instance 'move-use :user combatant :move move))
    (perform-move move battle combatant)))

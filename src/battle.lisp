
(in-package :rush)

(defclass battle ()
  ((combatants :reader combatants
               :initarg :combatants)
   (queued-events :accessor queued-events
                  :initform '())
   (remaining-hp-table :reader remaining-hp-table
                       :initform (make-hash-table))))

(defmethod next-events ((battle battle))
  (prog1 (reverse (queued-events battle))
    (setf (queued-events battle) '())))

(defun remaining-hp (battle combatant)
  (or (gethash combatant (remaining-hp-table battle))
      (setf (remaining-hp battle combatant)
            (max-hp combatant))))

(defun (setf remaining-hp) (amount battle combatant)
  (setf (gethash combatant (remaining-hp-table battle))
        amount))

(defmethod inflict-damage ((battle battle) combatant amount)
  (push (make-instance 'damage-infliction
                       :target combatant
                       :amount amount)
        (queued-events battle))
  (let ((remaining-hp (decf (remaining-hp battle combatant) amount)))
    (when (<= remaining-hp 0)
      (push (make-instance 'death
                           :target combatant)
            (queued-events battle)))))


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

(defun clamp (value bottom top)
  (min (max value bottom) top))

(defun deduct-hp (battle combatant amount)
  (setf (remaining-hp battle combatant)
        (clamp (- (remaining-hp battle combatant) amount)
               0
               (max-hp combatant))))

(defmethod heal-damage ((battle battle) combatant amount)
  (setf (remaining-hp battle combatant)
        (clamp (+ (remaining-hp battle combatant) amount)
               0
               (max-hp combatant)))
  (add-event battle (make-instance 'damage-heal
                                   :target combatant
                                   :amount amount)))

(defmethod add-event ((battle battle) event)
  (add-event (event-accumulator battle) event))

(defmethod inflict-damage ((battle battle) combatant amount)
  (add-event battle
             (make-instance 'damage-infliction
                            :target combatant
                            :amount amount))
  (let ((remaining-hp (deduct-hp battle combatant amount)))
    (when (<= remaining-hp 0)
      (add-event battle
                 (make-instance 'death
                                :target combatant)))))

(defmethod input-moves ((battle battle) combatant moves)
  (dolist (move-target-pair moves)
    (let ((move (car move-target-pair))
          (target (cdr move-target-pair)))
      (add-event battle (make-instance 'move-use :user combatant :move move))
      (perform-move move battle combatant target))))

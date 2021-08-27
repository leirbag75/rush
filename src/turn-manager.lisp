
(in-package :rush)

(defclass turn-manager ()
  ((combatants :reader combatants
               :writer initialize-combatants)
   (combatants-index :accessor combatants-index
                     :initform 0)))

(defmethod initialize-turn-manager ((turn-manager turn-manager) battle)
  (initialize-combatants (sort (coerce (all-combatants battle) 'vector)
                               #'> :key #'speed)
                         turn-manager)
  (set-next-to-move turn-manager battle))

(defmethod set-next-to-move ((turn-manager turn-manager) battle)
  (setf (next-combatant-to-move battle)
        (aref (combatants turn-manager) (combatants-index turn-manager)))
  (setf (combatants-index turn-manager)
        (mod (1+ (combatants-index turn-manager))
             (length (combatants turn-manager)))))

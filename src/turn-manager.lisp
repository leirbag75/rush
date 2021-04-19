
(in-package :rush)

(defclass turn-manager ()
  ((combatants :reader combatants
               :writer initialize-combatants)
   (combatants-index :accessor combatants-index
                     :initform 0)))

(defmethod initialize-instance :after ((turn-manager turn-manager)
                                       &key combatants)
  (initialize-combatants (coerce combatants 'vector)
                         turn-manager))

(defmethod next-player-to-move ((turn-manager turn-manager))
  (aref (combatants turn-manager) (combatants-index turn-manager)))

(defmethod end-turn ((turn-manager turn-manager))
  (setf (combatants-index turn-manager)
        (mod (1+ (combatants-index turn-manager))
             (length (combatants turn-manager)))))


(in-package :rush)

(defclass battle ()
  ((combatants :reader combatants
               :initarg :combatants)
   (queued-events :accessor queued-events
                  :initform '())))

(defmethod next-events ((battle battle))
  (reverse (queued-events battle)))

(defmethod inflict-damage ((battle battle) combatant amount)
  (push (make-instance 'damage-infliction
                       :target combatant
                       :amount amount)
        (queued-events battle)))
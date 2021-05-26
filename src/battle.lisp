
(in-package :rush)

(defclass battle ()
  ((combatants :reader combatants
               :initarg :combatants)
   (event-accumulator :reader event-accumulator
                      :initform (make-instance 'event-accumulator))
   (turn-manager :reader turn-manager
                 :writer initialize-turn-manager)
   (remaining-hp-table :reader remaining-hp-table
                       :initform (make-hash-table))
   (current-momentum-table :reader current-momentum-table
                           :initform (make-hash-table))))

(defmethod initialize-instance :after ((battle battle) &key combatants)
  (initialize-turn-manager (make-instance 'turn-manager :combatants combatants)
                           battle))

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

(defmethod next-player-to-move ((battle battle))
  (next-player-to-move (turn-manager battle)))

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

(defmethod input-moves ((battle battle) moves)
  (loop
    with user = (next-player-to-move battle)
    for move-target-pair in moves
    for move = (car move-target-pair)
    for target = (cdr move-target-pair)
    do
       (add-event battle (make-instance 'move-use :user user :move move))
       (perform-move move battle user target)
    finally (end-turn (turn-manager battle))))

(defmethod add-momentum ((battle battle) combatant amount)
  (incf (current-momentum battle combatant) amount)
  (add-event battle (make-instance 'momentum-gain
                                   :target combatant
                                   :amount amount)))

(defmethod current-momentum ((battle battle) combatant)
  (or (gethash combatant (current-momentum-table battle))
      (setf (current-momentum battle combatant) 0)))

(defun (setf current-momentum) (amount battle combatant)
  (setf (gethash combatant (current-momentum-table battle))
        amount))

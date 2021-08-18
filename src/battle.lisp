
(in-package :rush)

(defclass battle ()
  ((combatants :reader combatants
               :initarg :combatants)
   (event-accumulator :reader event-accumulator
                      :initform (make-instance 'event-accumulator))
   (next-player-to-move :accessor next-player-to-move)
   (turn-manager :reader turn-manager
                 :initarg :turn-manager)
   (remaining-hp-table :reader remaining-hp-table
                       :initform (make-hash-table))
   (current-momentum-table :reader current-momentum-table
                           :initform (make-hash-table))
   (rush-mode-table :reader rush-mode-table
                    :initform (make-hash-table))
   (available-actions-table :reader available-actions-table
                            :initform (make-hash-table))))

(defmethod initialize-instance :after ((battle battle) &key turn-manager)
  (initialize-turn-manager turn-manager battle))

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

(defmethod perform-move :before (move (battle battle) user targets)
  (add-event battle
             (make-instance 'move-use
                            :user user
                            :move move
                            :targets targets)))

(defmethod add-momentum ((battle battle) combatant amount)
  (incf (current-momentum battle combatant) amount)
  (add-event battle (make-instance 'momentum-gain
                                   :target combatant
                                   :amount amount))
  (when (>= (current-momentum battle combatant) 140)
    (setf (in-rush-mode-p battle combatant) t)
    (add-event battle (make-instance 'enter-rush-mode :target combatant))))

(defmethod current-momentum ((battle battle) combatant)
  (or (gethash combatant (current-momentum-table battle))
      (setf (current-momentum battle combatant) 0)))

(defun (setf current-momentum) (amount battle combatant)
  (setf (gethash combatant (current-momentum-table battle))
        amount))

(defun (setf in-rush-mode-p) (value battle combatant)
  (setf (gethash combatant (rush-mode-table battle)) value))

(defmethod in-rush-mode-p ((battle battle) combatant)
  (values (gethash combatant (rush-mode-table battle))))

(defmethod subtract-action ((battle battle) combatant &optional (amount 1))
  (setf (available-actions battle combatant)
        (max 0 (- (available-actions battle combatant) amount))))

(defmethod add-action ((battle battle) combatant &optional (amount 1))
  (incf (available-actions battle combatant) amount))

(defun (setf available-actions) (amount battle combatant)
  (setf (gethash combatant (available-actions-table battle)) amount))

(defmethod available-actions ((battle battle) combatant)
  (or (gethash combatant (available-actions-table battle))
      (setf (available-actions battle combatant) 2)))

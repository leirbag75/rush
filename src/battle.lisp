
(in-package :rush)

(defclass battle ()
  ((combatants :reader combatants
               :initarg :combatants)
   (incoming-event-accumulator :reader event-accumulator
                               :reader incoming-event-accumulator
                               :initform (make-instance 'event-accumulator))
   (outgoing-event-accumulator :reader outgoing-event-accumulator
                               :initform (make-instance 'event-accumulator))
   (next-combatant-to-move :accessor next-combatant-to-move)
   (turn-manager :reader turn-manager
                 :initarg :turn-manager)
   (remaining-hp-table :reader remaining-hp-table
                       :initform (make-hash-table))
   (current-momentum-table :reader current-momentum-table
                           :initform (make-hash-table))
   (rush-mode-table :reader rush-mode-table
                    :initform (make-hash-table))
   (available-actions-table :reader available-actions-table
                            :initform (make-hash-table))
   (deck-manager-table :reader deck-manager-table
                       :initform (make-hash-table))
   (subscribers :accessor subscribers
                :initform '()))
  (:default-initargs :turn-manager (make-instance 'turn-manager)))

(defmethod initialize-instance :after ((battle battle) &key turn-manager)
  (initialize-turn-manager turn-manager battle)
  (loop
    for combatant in (all-combatants battle)
    do (setf (get-deck-manager battle combatant)
             (make-instance 'deck-manager :deck (deck combatant)))))

(defmethod next-events ((battle battle))
  (next-events (outgoing-event-accumulator battle)))

(defmethod negate-event ((battle battle) event)
  (negate-event (incoming-event-accumulator battle) event))

(defmethod preempt-event ((battle battle) preempted-event preempting-event)
  (preempt-event (incoming-event-accumulator battle)
                 preempted-event
                 preempting-event))

(defmethod postempt-event ((battle battle) postempted-event postempting-event)
  (postempt-event (incoming-event-accumulator battle)
                  postempted-event
                  postempting-event))

(defmethod perform-event ((battle battle) (event negated-event))
  ;; Do nothing
  )

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
  (add-event battle (make-instance 'damage-heal
                                   :target combatant
                                   :amount amount)))

(defmethod perform-event ((battle battle) (event damage-heal))
  (let ((combatant (target event))
        (amount (amount event)))
    (setf (remaining-hp battle combatant)
          (clamp (+ (remaining-hp battle combatant) amount)
                 0
                 (max-hp combatant)))))

(defmethod add-event ((battle battle) event)
  (add-event (incoming-event-accumulator battle) event))

(defmethod commit-changes ((battle battle))
  (let ((incoming-event-accumulator (incoming-event-accumulator battle))
        (outgoing-event-accumulator (outgoing-event-accumulator battle)))
    (labels ((process-event (event)
               (loop
                 initially
                    (dolist (subscriber (reverse (subscribers battle)))
                      (notify subscriber event battle))
                 for next-event = (peek-event incoming-event-accumulator)
                 until (eql (unnegated next-event) event)
                 do
                    (process-event next-event)
                 finally
                    (let ((final-event (peek-event incoming-event-accumulator)))
                      (when (event-validp final-event battle)
                        (perform-event battle final-event)
                        (add-event outgoing-event-accumulator
                                   final-event))
                      (pop-event incoming-event-accumulator)))))
      (loop
        for event = (peek-event incoming-event-accumulator)
        while event
        do (process-event event)))))

(defmethod inflict-damage ((battle battle) combatant amount)
  (add-event battle
             (make-instance 'damage-infliction
                            :target combatant
                            :amount amount)))

(defmethod perform-event ((battle battle) (event damage-infliction))
  (let* ((combatant (target event))
         (amount (amount event))
         (remaining-hp (deduct-hp battle combatant amount)))
    (when (<= remaining-hp 0)
      (add-event battle
                 (make-instance 'death
                                :target combatant)))))

(defmethod perform-event ((battle battle) (event death)))

(defmethod perform-event ((battle battle) (event move-use))
  (perform-move (move event) battle (user event) (targets event)))

(defmethod add-momentum ((battle battle) combatant amount)
  (add-event battle (make-instance 'momentum-gain
                                   :target combatant
                                   :amount amount)))

(defmethod perform-event ((battle battle) (event momentum-gain))
  (let ((combatant (target event))
        (amount (amount event)))
    (incf (current-momentum battle combatant) amount)
    (when (>= (current-momentum battle combatant) 140)
      (add-event battle (make-instance 'enter-rush-mode :target combatant)))))

(defmethod perform-event ((battle battle) (event enter-rush-mode))
  (setf (in-rush-mode-p battle (target event)) t))

(defmethod reset-momentum ((battle battle) combatant)
  (add-event battle (make-instance 'momentum-reset :target combatant)))

(defmethod perform-event ((battle battle) (event momentum-reset))
  (setf (current-momentum battle (target event)) 0))

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

(defmethod leave-rush-mode ((battle battle) combatant)
  (when (in-rush-mode-p battle combatant)
    (add-event battle (make-instance 'exit-rush-mode :target combatant))))

(defmethod perform-event ((battle battle) (event exit-rush-mode))
  (setf (in-rush-mode-p battle (target event)) nil))

(defmethod subtract-action ((battle battle) combatant &optional (amount 1))
  (add-event battle
             (make-instance 'action-loss
                            :target combatant
                            :amount amount)))

(defmethod perform-event ((battle battle) (event action-loss))
  (let ((combatant (target event))
        (amount (amount event)))
    (setf (available-actions battle combatant)
          (max 0 (- (available-actions battle combatant) amount)))))

(defmethod add-action ((battle battle) combatant &optional (amount 1))
  (add-event battle
             (make-instance 'action-gain
                            :target combatant
                            :amount amount)))

(defmethod perform-event ((battle battle) (event action-gain))
  (let ((combatant (target event))
        (amount (amount event)))
    (incf (available-actions battle combatant) amount)))

(defun (setf available-actions) (amount battle combatant)
  (setf (gethash combatant (available-actions-table battle)) amount))

(defmethod available-actions ((battle battle) combatant)
  (or (gethash combatant (available-actions-table battle))
      (setf (available-actions battle combatant) 2)))

;; Don't normally use names starting with "get," but unfortunately,
;; deck-manager is already being used as a reader for an error class
(defun get-deck-manager (battle combatant)
  (values (gethash combatant (deck-manager-table battle))))

(defun (setf get-deck-manager) (deck-manager battle combatant)
  (setf (gethash combatant (deck-manager-table battle)) deck-manager))

(defun draw (battle combatant)
  (hand (get-deck-manager battle combatant)))

(defmethod all-combatants ((battle battle))
  (reduce #'append (combatants battle) :from-end t))

(defmethod allies ((battle battle) combatant)
  (find-if (lambda (team) (member combatant team))
           (combatants battle)))

(defmethod enemies ((battle battle) combatant)
  (reduce #'append
          (remove-if (lambda (team) (member combatant team))
                     (combatants battle))
          :from-end t))

(defmethod subscribe ((battle battle) subscriber)
  (push subscriber (subscribers battle)))

(defmethod unsubscribe ((battle battle) subscriber)
  (setf (subscribers battle) (remove subscriber (subscribers battle))))

(defmethod combatant-deadp ((battle battle) combatant)
  (zerop (remaining-hp battle combatant)))

(defmethod combatant-incapacitatedp ((battle battle) combatant)
  (combatant-deadp battle combatant))

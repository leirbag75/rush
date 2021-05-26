
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

(defun make-test-battle ()
  (let* ((combatant1 (make-instance 'mock-combatant))
         (combatant2 (make-instance 'mock-combatant))
         (battle (make-battle-with-combatants combatant1 combatant2)))
    (values battle combatant1 combatant2)))

(deftest subtracts-hp (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (let ((remaining-hp (- *mock-combatant-max-hp* 10)))
      (inflict-damage battle combatant 10)
      (assert-eql remaining-hp (remaining-hp battle combatant)))))

(deftest keeps-hp-at-least-0 (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (inflict-damage battle combatant (* 2 *mock-combatant-max-hp*))
    (assert-eql 0 (remaining-hp battle combatant))))

(deftest adds-damage-infliction-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (inflict-damage battle combatant 10)
    (assert-events-match battle
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount 10))))

(deftest adds-hp (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (let ((remaining-hp (* *mock-combatant-max-hp* 3/4)))
      (inflict-damage battle combatant (/ *mock-combatant-max-hp* 2))
      (heal-damage battle combatant (/ *mock-combatant-max-hp* 4))
      (assert-eql remaining-hp (remaining-hp battle combatant)))))

(deftest keeps-hp-at-most-max (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (heal-damage battle combatant 10)
    (assert-eql *mock-combatant-max-hp* (remaining-hp battle combatant))))

(deftest adds-damage-heal-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (heal-damage battle combatant 10)
    (assert-events-match battle
                         (make-instance 'damage-heal
                                        :target combatant
                                        :amount 10))))

(deftest battle-death (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (inflict-damage battle combatant *mock-combatant-max-hp*)
    (assert-events-match battle
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount *mock-combatant-max-hp*)
                         (make-instance 'death :target combatant))))

(defclass mock-move ()
  ((body :reader body
         :initarg :body)))

(defmethod perform-move ((move mock-move) battle combatant target)
  (funcall (body move) battle combatant target))

(deftest should-add-use-move-event (test-battle)
  (let* ((battle (make-test-battle))
         (combatant (next-player-to-move battle))
         (move (make-instance 'mock-move
                              :body (lambda (&rest args)
                                      (declare (ignore args))))))
    (input-moves battle (list (cons move combatant)))
    (assert-events-match battle
                         (make-instance 'move-use
                                        :move move
                                        :user combatant))))

(deftest should-call-perform-move (test-battle)
  (should-be-evaluated (-->this) ("perform-move not called")
    (let* ((battle (make-test-battle))
           (combatant (next-player-to-move battle))
           (move (make-instance 'mock-move
                                :body (lambda (&rest args)
                                        (declare (ignore args))
                                        -->this))))
      (input-moves battle (list (cons move combatant))))))

(deftest adds-momentum (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant 20)
    (assert-eql 20 (current-momentum battle combatant))))

(deftest adds-momentum-gain-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant 20)
    (assert-events-match battle
                         (make-instance 'momentum-gain
                                        :target combatant
                                        :amount 20))))

(deftest enters-rush-mode (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant 140)
    (unless (in-rush-mode-p battle combatant)
      (error "Combatant did not enter rush mode"))))

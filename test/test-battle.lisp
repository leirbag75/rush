
(in-package :rush)

(defvar *mock-combatant-number* 0)

(defparameter *mock-combatant-max-hp* 200)

(define-symbol-macro *weak-damage* (floor *mock-combatant-max-hp* 10))

(defparameter *max-momentum* 140)

(define-symbol-macro *small-momentum* (floor *max-momentum* 10))

(defparameter *default-available-actions* 2)

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
    (let ((remaining-hp (- *mock-combatant-max-hp* *weak-damage*)))
      (inflict-damage battle combatant *weak-damage*)
      (assert-eql remaining-hp (remaining-hp battle combatant)))))

(deftest keeps-hp-at-least-0 (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (inflict-damage battle combatant (* 2 *mock-combatant-max-hp*))
    (assert-eql 0 (remaining-hp battle combatant))))

(deftest adds-damage-infliction-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (inflict-damage battle combatant *weak-damage*)
    (assert-events-match battle
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount *weak-damage*))))

(deftest adds-hp (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (let ((remaining-hp (* *mock-combatant-max-hp* 3/4)))
      (inflict-damage battle combatant (/ *mock-combatant-max-hp* 2))
      (heal-damage battle combatant (/ *mock-combatant-max-hp* 4))
      (assert-eql remaining-hp (remaining-hp battle combatant)))))

(deftest keeps-hp-at-most-max (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (heal-damage battle combatant *weak-damage*)
    (assert-eql *mock-combatant-max-hp* (remaining-hp battle combatant))))

(deftest adds-damage-heal-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (heal-damage battle combatant *weak-damage*)
    (assert-events-match battle
                         (make-instance 'damage-heal
                                        :target combatant
                                        :amount *weak-damage*))))

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
    (perform-move move battle (next-player-to-move battle) (list combatant))
    (assert-events-match battle
                         (make-instance 'move-use
                                        :move move
                                        :user combatant
                                        :targets (list combatant)))))

(deftest should-call-perform-move (test-battle)
  (should-be-evaluated (-->this) ("perform-move not called")
    (let* ((battle (make-test-battle))
           (combatant (next-player-to-move battle))
           (move (make-instance 'mock-move
                                :body (lambda (&rest args)
                                        (declare (ignore args))
                                        -->this))))
      (input-moves battle (list (cons move (list combatant)))))))

(deftest adds-momentum (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant *small-momentum*)
    (assert-eql *small-momentum* (current-momentum battle combatant))))

(deftest adds-momentum-gain-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant *small-momentum*)
    (assert-events-match battle
                         (make-instance 'momentum-gain
                                        :target combatant
                                        :amount *small-momentum*))))

(deftest enters-rush-mode (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant *max-momentum*)
    (unless (in-rush-mode-p battle combatant)
      (error "Combatant did not enter rush mode"))))

(deftest adds-enter-rush-mode-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant *max-momentum*)
    (assert-events-match battle
                         (make-instance 'momentum-gain
                                        :target combatant
                                        :amount *max-momentum*)
                         (make-instance 'enter-rush-mode
                                        :target combatant))))

(deftest subtracts-action (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (subtract-action battle combatant)
    (assert-eql (1- *default-available-actions*)
                (available-actions battle combatant))))

(deftest actions-stay-at-least-zero (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (subtract-action battle combatant (1+ *default-available-actions*))
    (assert-eql 0 (available-actions battle combatant))))


(in-package :rush)

(defvar *mock-combatant-number* 0)

(defparameter *mock-combatant-max-hp* 200)

(defparameter *mock-combatant-deck* '(1 2 3 4 5 6 7 8 9 10))

(define-symbol-macro *weak-damage* (floor *mock-combatant-max-hp* 10))

(defparameter *max-momentum* 140)

(define-symbol-macro *small-momentum* (floor *max-momentum* 10))

(defparameter *default-available-actions* 2)

(defclass mock-combatant ()
  ((name :reader name
         :initform (format nil "Mock ~A" (incf *mock-combatant-number*)))))

(defmethod max-hp ((combatant mock-combatant))
  *mock-combatant-max-hp*)

(defmethod deck ((combatant mock-combatant))
  *mock-combatant-deck*)

(defun make-battle-with-combatants (&rest combatants)
  (make-instance 'battle
                 :combatants (mapcar #'list combatants)
                 :turn-manager (make-instance 'mock-turn-manager)))

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
      (perform-event battle
                     (make-instance 'damage-infliction
                                    :target combatant
                                    :amount *weak-damage*))
      (assert-eql remaining-hp (remaining-hp battle combatant)))))

(deftest keeps-hp-at-least-0 (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'damage-infliction
                                  :target combatant
                                  :amount (* 2 *mock-combatant-max-hp*)))
    (assert-eql 0 (remaining-hp battle combatant))))

(deftest adds-damage-infliction-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (inflict-damage battle combatant *weak-damage*)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount *weak-damage*))))

(deftest adds-hp (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (let ((remaining-hp (* *mock-combatant-max-hp* 3/4)))
      (perform-event battle
                     (make-instance 'damage-infliction
                                    :target combatant
                                    :amount (/ *mock-combatant-max-hp* 2)))
      (perform-event battle
                     (make-instance 'damage-heal
                                    :target combatant
                                    :amount (/ *mock-combatant-max-hp* 4)))
      (assert-eql remaining-hp (remaining-hp battle combatant)))))

(deftest keeps-hp-at-most-max (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'damage-infliction
                                  :target combatant
                                  :amount *weak-damage*))
    (perform-event battle
                   (make-instance 'damage-heal
                                  :target combatant
                                  :amount (* 2 *weak-damage*)))
    (assert-eql *mock-combatant-max-hp* (remaining-hp battle combatant))))

(deftest adds-damage-heal-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (heal-damage battle combatant *weak-damage*)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'damage-heal
                                        :target combatant
                                        :amount *weak-damage*))))

(deftest battle-death (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'damage-infliction
                                  :target combatant
                                  :amount *mock-combatant-max-hp*))
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'death :target combatant))))

(deftest combatant-dies (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (when (combatant-deadp battle combatant)
      (error "Combatant dead without doing anything"))
    (perform-event battle
                   (make-instance 'damage-infliction
                                  :target combatant
                                  :amount *mock-combatant-max-hp*))
    (commit-changes battle)
    (unless (combatant-deadp battle combatant)
      (error "Combatant not counted as dead"))))

(deftest dead-combatant-considered-incapacitated (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (when (combatant-incapacitatedp battle combatant)
      (error "Combatant incapacitated before doing anything"))
    (perform-event battle
                   (make-instance 'damage-infliction
                                  :target combatant
                                  :amount *mock-combatant-max-hp*))
    (unless (combatant-incapacitatedp battle combatant)
      (error "Dead combatant not considered incapacitated"))))

(defclass mock-move (mock)
  ())

(define-mock-method perform-move ((_ mock-move) battle combatant target))

(deftest should-call-perform-move (test-battle)
  (with-mock (move mock-move :error-message "perform-move not called")
    (multiple-value-bind (battle combatant) (make-test-battle)
      (perform-event battle
                     (make-instance 'move-use
                                    :user combatant
                                    :targets '()
                                    :move move)))))

(deftest adds-momentum (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'momentum-gain
                                  :target combatant
                                  :amount *small-momentum*))
    (assert-eql *small-momentum* (current-momentum battle combatant))))

(deftest adds-momentum-gain-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-momentum battle combatant *small-momentum*)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'momentum-gain
                                        :target combatant
                                        :amount *small-momentum*))))

(deftest enters-rush-mode (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle (make-instance 'enter-rush-mode :target combatant))
    (unless (in-rush-mode-p battle combatant)
      (error "Combatant did not enter rush mode"))))

(deftest adds-enter-rush-mode-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'momentum-gain
                                  :target combatant
                                  :amount *max-momentum*))
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'enter-rush-mode
                                        :target combatant))))

(deftest subtracts-action (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle (make-instance 'action-loss :target combatant))
    (assert-eql (1- *default-available-actions*)
                (available-actions battle combatant))))

(deftest adds-action-loss-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (subtract-action battle combatant)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'action-loss
                                        :target combatant))))

(deftest actions-stay-at-least-zero (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'action-loss
                                  :target combatant
                                  :amount  (1+ *default-available-actions*)))
    (assert-eql 0 (available-actions battle combatant))))

(deftest adds-action (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'action-gain :target combatant))
    (assert-eql (1+ *default-available-actions*)
                (available-actions battle combatant))))

(deftest adds-action-gain-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (add-action battle combatant)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'action-gain
                                        :target combatant))))

(defclass mock-turn-manager (mock)
  ())

(define-mock-method initialize-turn-manager ((_ mock-turn-manager) battle))

(deftest calls-initialize-turn-manager (test-battle)
  (with-mock (turn-manager mock-turn-manager
              :error-message "initialize-turn-manager not called")
    (let ((combatant (make-mock-combatant)))
      (make-instance 'battle
                     :combatants (list (list combatant))
                     :turn-manager turn-manager))))

(defclass mock-subscriber (mock)
  ())

(define-mock-method notify ((_ mock-subscriber) event battle))

(defclass mock-event (mock)
  ())

(define-mock-method perform-event (battle (_ mock-event)))

(deftest calls-notify (test-battle)
  (with-mock (subscriber mock-subscriber :error-message "notify not called")
    (let ((battle (make-test-battle)))
      (subscribe battle subscriber)
      (add-event battle (make-instance 'mock-event))
      (commit-changes battle))))

(deftest lets-subscribers-unsubscribe (test-battle)
  (with-mock (subscriber mock-subscriber
              :times 0
              :error-message "Failed to unsubscribe")
    (let ((battle (make-test-battle)))
      (subscribe battle subscriber)
      (unsubscribe battle subscriber)
      (add-event battle (make-instance 'mock-event))
      (commit-changes battle))))

(deftest leave-rush-mode-null-case (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (leave-rush-mode battle combatant)
    (commit-changes battle)
    ;; Shouldn't have any events
    (assert-events-match battle)))

(deftest leave-rush-mode-base-case (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle (make-instance 'enter-rush-mode :target combatant))
    (leave-rush-mode battle combatant)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'exit-rush-mode :target combatant))))

(deftest adds-reset-momentum-event (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (reset-momentum battle combatant)
    (commit-changes battle)
    (assert-events-match battle
                         (make-instance 'momentum-reset :target combatant))))

(deftest sets-momentum-to-0 (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle
                   (make-instance 'momentum-gain
                                  :target combatant
                                  :amount *small-momentum*))
    (perform-event battle (make-instance 'momentum-reset :target combatant))
    (assert-eql 0 (current-momentum battle combatant))))

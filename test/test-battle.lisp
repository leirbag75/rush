
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
                 :turn-manager (make-instance
                                'mock-turn-manager
                                :body (lambda (&rest args)
                                        (declare (ignore args))))))

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
    (assert-events-match battle
                         (make-instance 'death :target combatant))))

(defclass mock-move ()
  ((body :reader body
         :initarg :body)))

(defmethod perform-move ((move mock-move) battle combatant target)
  (funcall (body move) battle combatant target))

(deftest should-call-perform-move (test-battle)
  (should-be-evaluated (-->this) ("Perform-move not called")
    (multiple-value-bind (battle combatant) (make-test-battle)
      (let ((move (make-instance 'mock-move
                                 :body (lambda (&rest args)
                                         (declare (ignore args))
                                         -->this))))
        (perform-event battle
                       (make-instance 'move-use
                                      :user combatant
                                      :targets '()
                                      :move move))))))

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
    (assert-events-match battle
                         (make-instance 'action-gain
                                        :target combatant))))

(defclass mock-turn-manager ()
  ((body :reader body
         :initarg :body)))

(defmethod initialize-turn-manager ((turn-manager mock-turn-manager) battle)
  (funcall (body turn-manager) turn-manager battle))

(deftest calls-initialize-turn-manager (test-battle)
  (should-be-evaluated (-->this) ("initialize-turn-manager not called")
    (let* ((turn-manager (make-instance 'mock-turn-manager
                                        :body (lambda (&rest args)
                                                (declare (ignore args))
                                                -->this)))
           (combatant (make-mock-combatant)))
      (make-instance 'battle
                     :combatants (list (list combatant))
                     :turn-manager turn-manager))))

(defclass mock-subscriber ()
  ((body :accessor body
         :initarg :body)))

(defmethod notify ((subscriber mock-subscriber) event battle)
  (funcall (body subscriber) subscriber event battle))

;; To allow the objects to be viewed in the debugger
(define-condition notify-test-failed (error)
  ((battle :reader battle
           :initarg :battle)
   (subscriber :reader subscriber
               :initarg :subscriber)))

(deftest calls-notify (test-battle)
  (let ((battle (make-test-battle))
        (subscriber (make-instance 'mock-subscriber)))
    (should-be-evaluated (-->this) ('notify-test-failed
                                    :battle battle
                                    :subscriber subscriber)
      (setf (body subscriber)
            (lambda (&rest args) (declare (ignore args)) -->this))
      (subscribe battle subscriber)
      (add-event battle (make-instance 'event)))))

(deftest leave-rush-mode-null-case (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (leave-rush-mode battle combatant)
    ;; Shouldn't have any events
    (assert-events-match battle)))

(deftest leave-rush-mode-base-case (test-battle)
  (multiple-value-bind (battle combatant) (make-test-battle)
    (perform-event battle (make-instance 'enter-rush-mode :target combatant))
    (leave-rush-mode battle combatant)
    (assert-events-match battle
                         (make-instance 'exit-rush-mode :target combatant))))

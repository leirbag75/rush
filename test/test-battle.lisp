
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

(deftest subtracts-hp (test-battle)
  (let* ((combatant (make-mock-combatant))
         (remaining-hp (- *mock-combatant-max-hp* 10))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant 10)
    (unless (eql (remaining-hp battle combatant) remaining-hp)
      (error 'objects-dont-match
             :expected remaining-hp
             :actual (remaining-hp battle combatant)))))

(deftest keeps-hp-at-least-0 (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant (* 2 *mock-combatant-max-hp*))
    (unless (eql (remaining-hp battle combatant) 0)
      (error 'objects-dont-match
             :expected 0
             :actual (remaining-hp battle combatant)))))

(deftest adds-damage-infliction-event (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant 10)
    (assert-events-match battle
                         (make-instance 'damage-infliction
                                        :target combatant
                                        :amount 10))))

(deftest adds-hp (test-battle)
  (let* ((combatant (make-mock-combatant))
         (remaining-hp (* *mock-combatant-max-hp* 3/4))
         (battle (make-battle-with-combatants combatant)))
    (inflict-damage battle combatant (/ *mock-combatant-max-hp* 2))
    (heal-damage battle combatant (/ *mock-combatant-max-hp* 4))
    (unless (eql (remaining-hp battle combatant) remaining-hp)
      (error 'objects-dont-match
             :expected remaining-hp
             :actual (remaining-hp battle combatant)))))

(deftest keeps-hp-at-most-max (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (heal-damage battle combatant 10)
    (unless (eql (remaining-hp battle combatant) *mock-combatant-max-hp*)
      (error 'objects-dont-match
             :expected *mock-combatant-max-hp*
             :actual (remaining-hp battle combatant)))))

(deftest adds-damage-heal-event (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
    (heal-damage battle combatant 10)
    (assert-events-match battle
                         (make-instance 'damage-heal
                                        :target combatant
                                        :amount 10))))

(deftest battle-death (test-battle)
  (let* ((combatant (make-mock-combatant))
         (battle (make-battle-with-combatants combatant)))
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
  (let* ((combatant (make-mock-combatant))
         (move (make-instance 'mock-move
                              :body (lambda (&rest args)
                                      (declare (ignore args)))))
         (battle (make-battle-with-combatants combatant)))
    (input-moves battle combatant (list (cons move combatant)))
    (assert-events-match battle
                         (make-instance 'move-use
                                        :move move
                                        :user combatant))))

(deftest should-call-perform-move (test-battle)
  (should-be-evaluated (-->this) ("perform-move not called")
    (let* ((combatant (make-mock-combatant))
           (move (make-instance 'mock-move
                                :body (lambda (&rest args)
                                        (declare (ignore args))
                                        -->this)))
           (battle (make-battle-with-combatants combatant)))
      (input-moves battle combatant (list (cons move combatant))))))

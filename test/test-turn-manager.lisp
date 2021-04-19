
(in-package :rush)

(deftest same-player-shouldnt-get-consecutive-turns (test-turn-manager)
  (let* ((combatant1 (make-instance 'standard-object))
         (combatant2 (make-instance 'standard-object))
         (turn-manager (make-instance 'turn-manager
                                      :combatants (list combatant1
                                                        combatant2)))
         (first-to-act (next-player-to-move turn-manager))
         (_ (end-turn turn-manager))
         (second-to-act (next-player-to-move turn-manager)))
    (declare (ignore _))
    (when (eql first-to-act second-to-act)
      (error "Same player moved consecutively"))))

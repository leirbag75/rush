
(in-package :rush)

(defmethod speed ((combatant mock-combatant))
  7)

(deftest turns-come-in-order (test-turn-manager)
  (let* ((combatant1 (make-instance 'mock-combatant))
         (combatant2 (make-instance 'mock-combatant))
         (turn-manager (make-instance 'turn-manager))
         (battle (make-instance 'battle
                                :combatants (list (list combatant1)
                                                  (list combatant2))
                                :turn-manager turn-manager)))
    (loop
      for i from 1 to 4
      do (set-next-to-move turn-manager battle)
      collect (next-combatant-to-move battle) into turns
      finally (assert-equal (list combatant1 combatant2 combatant1 combatant2)
                            turns))))

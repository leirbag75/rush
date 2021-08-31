
(in-package :rush)

(defgeneric remaining-hp (battle combatant))

(defgeneric inflict-damage (battle combatant amount))

(defgeneric heal-damage (battle combatant amount))

(defgeneric add-momentum (battle combatant amount))

(defgeneric reset-momentum (battle combatant))

(defgeneric current-momentum (battle combatant))

(defgeneric in-rush-mode-p (battle combatant))

(defgeneric leave-rush-mode (battle combatant))

(defgeneric subtract-action (battle combatant &optional actions))

(defgeneric add-action (battle combatant &optional actions))

(defgeneric available-actions (battle combatant))

(defgeneric perform-event (battle event))

(defgeneric commit-changes (battle))

(defgeneric all-combatants (battle))

(defgeneric allies (battle combatant))

(defgeneric allies-not-self (battle combatant)
  (:method (battle combatant)
    (remove combatant (allies battle combatant))))

(defgeneric enemies (battle combatant))

(defgeneric subscribe (battle subscriber))

(defgeneric unsubscribe (battle subscriber))

(defgeneric combatant-deadp (battle combatant))

(defgeneric combatant-incapacitatedp (battle combatant))

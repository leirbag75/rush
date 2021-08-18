
(in-package :rush)

(defgeneric remaining-hp (battle combatant))

(defgeneric inflict-damage (battle combatant amount))

(defgeneric heal-damage (battle combatant amount))

(defgeneric add-momentum (battle combatant amount))

(defgeneric current-momentum (battle combatant))

(defgeneric in-rush-mode-p (battle combatant))

(defgeneric subtract-action (battle combatant &optional actions))

(defgeneric available-actions (battle combatant))

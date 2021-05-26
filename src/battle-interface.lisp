
(in-package :rush)

(defgeneric remaining-hp (battle combatant))

(defgeneric inflict-damage (battle combatant amount))

(defgeneric heal-damage (battle combatant amount))

(defgeneric input-moves (battle moves))

(defgeneric add-momentum (battle combatant amount))

(defgeneric current-momentum (battle combatant))

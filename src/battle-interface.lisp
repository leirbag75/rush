
(in-package :rush)

(defgeneric remaining-hp (battle combatant))

(defgeneric inflict-damage (battle combatant amount))

(defgeneric heal-damage (battle combatant amount))

(defgeneric input-moves (battle combatant moves))


(in-package :rush)

(defgeneric shuffle-deck (deck-manager))

(defgeneric discard-card (deck-manager card))

(defgeneric hand (deck-manager))

(defgeneric remaining-deck (deck-manager))

(defgeneric draw (deck-manager))

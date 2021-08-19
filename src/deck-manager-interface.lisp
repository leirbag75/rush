
(in-package :rush)

(defgeneric shuffle-deck (deck-manager))

;; With use-card, the card must be in the user's hand. With
;; discard-card, any arbitrary card can be discarded.

(defgeneric use-card (deck-manager card))

(defgeneric discard-card (deck-manager card))

(defgeneric hand (deck-manager))

(defgeneric draw (deck-manager))

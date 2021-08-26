
(in-package :rush)

(defgeneric selectable-cards (move-selector))

(defgeneric selectable-specials (move-selector))

(defgeneric remaining-move-selections (move-selector))

(defgeneric start-target-selection (move-selector move))

(defgeneric select-move (move-selector move targets))

(defgeneric selected-moves (move-selector))

(defgeneric unselect-move (move-selector))

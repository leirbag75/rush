
(in-package :rush)

(defgeneric selectable-cards (move-selector))

(defgeneric selectable-specials (move-selector))

(defgeneric remaining-move-selections (move-selector))

(defgeneric select-move (move-selector move targets))

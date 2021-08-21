
(in-package :rush)

(defgeneric selectable-cards (move-selector))

(defgeneric selectable-specials (move-selector))

(defgeneric remaining-selections (move-selector))

(defgeneric select (move-selector move))

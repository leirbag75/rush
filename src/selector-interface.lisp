
(in-package :rush)

(defgeneric select (selector item))

(defgeneric unselect (selector))

(defgeneric selected-items (selector))

(defgeneric selector-fullp (selector))

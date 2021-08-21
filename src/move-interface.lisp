
(in-package :rush)

(defgeneric perform-move (move battle user targets))

(defgeneric available-targets (move battle user))

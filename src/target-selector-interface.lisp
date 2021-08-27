
(in-package :rush)

(defgeneric selectable-targets (target-selector))

(defgeneric remaining-target-selections (target-selector))

(defgeneric select-target (target-selector target))

(defgeneric selected-targets (target-selector))

(defgeneric unselect-target (target-selector))

(defgeneric finalize-target-selection (target-selector))


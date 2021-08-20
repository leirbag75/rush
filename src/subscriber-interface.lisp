
(in-package :rush)

(defgeneric notify (subscriber event battle)
  (:method (subscriber event battle)
    ;; Do nothing by default
    ))

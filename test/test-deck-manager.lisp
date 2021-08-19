
(in-package :rush)

(defparameter *test-hand-size* 3)

(defparameter *mock-deck* '(1 2 3 4 5 6))

(deftest use-shuffle-algorithm (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size *test-hand-size*
                                     :deck *mock-deck*
                                     :shuffle-algorithm #'reverse)))
    (assert-equal (reverse *mock-deck*)
                  (remaining-deck deck-manager))))


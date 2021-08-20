
(in-package :rush)

(defparameter *test-hand-size* 3)

(defparameter *mock-deck* '(1 2 3 4 5 6))

(defparameter *card-not-in-deck* (gensym))

(deftest use-shuffle-algorithm (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size *test-hand-size*
                                     :deck *mock-deck*
                                     :shuffle-algorithm #'reverse)))
    (assert-equal (reverse *mock-deck*)
                  (remaining-deck deck-manager))))

(deftest draw-first-cards (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size *test-hand-size*
                                     :deck *mock-deck*
                                     :shuffle-algorithm #'identity)))
    (assert-equal (subseq *mock-deck* 0 *test-hand-size*)
                  (coerce (hand deck-manager) 'list))))

(deftest discard-card-not-in-deck (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size *test-hand-size*
                                     :deck *mock-deck*
                                     :shuffle-algorithm #'identity)))
    (should-signal (card-not-in-deck) ("Did not throw error")
      (discard-card deck-manager *card-not-in-deck*))))

(deftest discard-card-base-case (test-deck-manager)
  (let* ((deck-manager (make-instance 'deck-manager
                                      :hand-size *test-hand-size*
                                      :deck *mock-deck*))
         (deck (remaining-deck deck-manager)))
    (discard-card deck-manager (first *mock-deck*))
    (assert-equal (remove (first *mock-deck*) deck)
                  (remaining-deck deck-manager))))

(deftest add-card-to-discard-pile (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size *test-hand-size*
                                     :deck *mock-deck*)))
    (discard-card deck-manager (first *mock-deck*))
    (assert-equal (list (first *mock-deck*))
                  (discard-pile deck-manager))))

(deftest recycles-cards-when-not-enough (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size 5
                                     :deck '(1 2 3 4 5 6)
                                     :shuffle-algorithm #'identity)))
    (discard-card deck-manager 1)
    (discard-card deck-manager 2)
    (assert-equal '(3 4 5 6 2)
                  (hand deck-manager))))

(deftest removes-discard-pile-when-recycling (test-deck-manager)
  (let ((deck-manager (make-instance 'deck-manager
                                     :hand-size 5
                                     :deck '(1 2 3 4 5 6)
                                     :shuffle-algorithm #'identity)))
    (discard-card deck-manager 1)
    (discard-card deck-manager 2)
    (hand deck-manager)
    (assert-equal '()
                  (discard-pile deck-manager))))

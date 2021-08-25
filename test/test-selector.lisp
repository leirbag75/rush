
(in-package :rush)

(deftest select-too-many (test-selector)
  (let ((selector (make-instance 'selector
                                 :valid-options '(1 2 3)
                                 :selection-count 0)))
    (should-signal (selector-full) ("Selector full error not thrown")
      (select selector 1))))

(deftest select-invalid (test-selector)
  (let ((selector (make-instance 'selector
                                 :valid-options '(1 2 3)
                                 :selection-count 2)))
    (should-signal (invalid-selection) ("Invalid selection error not thrown")
      (select selector 4))))

(deftest select-base-case (test-selector)
  (let ((selector (make-instance 'selector
                                 :valid-options '(1 2 3)
                                 :selection-count 2)))
    (select selector 2)
    (assert-equal '(2)
                  (selected-items selector))))

(deftest select-twice (test-selector)
  (let ((selector (make-instance 'selector
                                 :valid-options '(1 2 3)
                                 :selection-count 2)))
    (should-signal (invalid-selection) ("Invalid selection error not thrown")
      (select selector 2)
      (select selector 2))))

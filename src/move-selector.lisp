
(in-package :rush)

(defclass move-selector ()
  ((selector :reader selector
             :writer initialize-selector)
   (selectable-cards :reader %selectable-cards
                     :initarg :selectable-cards)
   (selectable-specials :reader %selectable-specials
                        :initarg :selectable-specials)))

(defmethod selectable-cards ((move-selector move-selector))
  (remove-if-not (lambda (card)
                   (member card (valid-options (selector move-selector))))
                 (%selectable-cards move-selector)))

(defmethod selectable-specials ((move-selector move-selector))
  (remove-if-not (lambda (special)
                   (member special (valid-options (selector move-selector))))
                 (%selectable-specials move-selector)))

(defmethod select-move ((move-selector move-selector) move targets)
  (select (selector move-selector) (cons move targets)))

(defmethod unselect-move ((move-selector move-selector))
  (unselect (selector move-selector)))

(defmethod initialize-instance :after ((move-selector move-selector)
                                       &key
                                         selectable-cards selectable-specials
                                         selection-count)
  (initialize-selector (make-instance 'selector
                                      :valid-options (append
                                                      selectable-cards
                                                      selectable-specials)
                                      :selection-count selection-count
                                      :key #'car)
                       move-selector))

(defmethod remaining-move-selections ((move-selector move-selector))
  (remaining-selections (selector move-selector)))

(defmethod selected-moves ((move-selector move-selector))
  (selected-items (selector move-selector)))

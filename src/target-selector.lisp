
(in-package :rush)

(defclass target-selector ()
  ((selector :reader selector
             :writer initialize-selector)
   (move :reader move
         :initarg :move)
   (move-selector :reader move-selector
                  :initarg :move-selector)))

(defmethod initialize-instance :after ((target-selector target-selector)
                                       &key
                                         selectable-targets selection-count)
  (initialize-selector (make-instance 'selector
                                      :valid-options selectable-targets
                                      :selection-count selection-count)
                       target-selector))

(defmethod selectable-targets ((target-selector target-selector))
  (valid-options (selector target-selector)))

(defmethod remaining-target-selections ((target-selector target-selector))
  (remaining-selections (selector target-selector)))

(defmethod selected-targets ((target-selector target-selector))
  (selected-items (selector target-selector)))

(defmethod select-target ((target-selector target-selector) target)
  (select (selector target-selector) target))

(defmethod unselect-target ((target-selector target-selector))
  (unselect (selector target-selector)))

(defmethod finalize-target-selection ((target-selector target-selector))
  (select-move (move-selector target-selector)
               (move target-selector)
               (selected-targets target-selector)))

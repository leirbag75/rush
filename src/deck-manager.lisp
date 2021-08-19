
(in-package :rush)

(defun shuffle (deck)
  (flet ((copy-or-coerce (sequence)
           (etypecase sequence
             (list (coerce sequence 'vector))
             (vector (copy-seq sequence)))))
    (loop
      with result = (copy-or-coerce deck)
      for k from (1- (length result)) downto 0
      for to-switch = (random (1+ k))
      do
         (let ((old (aref result k)))
           (setf (aref result k) (aref result to-switch))
           (setf (aref result to-switch) old))
      finally (return (coerce result (type-of deck))))))

(defclass deck-manager ()
  ((hand-size :reader hand-size
              :initarg :hand-size)
   (remaining-deck :accessor remaining-deck)
   (shuffle-algorithm :reader shuffle-algorithm
                      :initarg :shuffle-algorithm)))

(defmethod initialize-instance :after ((deck-manager deck-manager)
                                       &key deck shuffle-algorithm)
  (setf (remaining-deck deck-manager) (funcall shuffle-algorithm deck)))

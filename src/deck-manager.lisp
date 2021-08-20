
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

(defvar *default-hand-size* 5)

(defclass deck-manager ()
  ((hand-size :reader hand-size
              :initarg :hand-size)
   (remaining-deck :accessor remaining-deck)
   (shuffle-algorithm :reader shuffle-algorithm
                      :initarg :shuffle-algorithm))
  (:default-initargs
   :shuffle-algorithm #'shuffle
   :hand-size *default-hand-size*))

(defmethod hand ((deck-manager deck-manager))
  (subseq (remaining-deck deck-manager) 0 (hand-size deck-manager)))

(defmethod hand-size ((deck-manager deck-manager))
  (array-dimension (hand deck-manager) 0))

(defmethod initialize-instance :after ((deck-manager deck-manager)
                                       &key deck shuffle-algorithm hand-size)
  (setf (remaining-deck deck-manager) (funcall shuffle-algorithm deck)))


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
   (discard-pile :accessor discard-pile
                 :initform '())
   (shuffle-algorithm :reader shuffle-algorithm
                      :initarg :shuffle-algorithm))
  (:default-initargs
   :shuffle-algorithm #'shuffle
   :hand-size *default-hand-size*))

(defmethod hand ((deck-manager deck-manager))
  (subseq (remaining-deck deck-manager) 0 (hand-size deck-manager)))

(defmethod initialize-instance :after ((deck-manager deck-manager)
                                       &key deck shuffle-algorithm hand-size)
  (setf (remaining-deck deck-manager) (funcall shuffle-algorithm deck)))

(define-condition card-not-in-deck (error)
  ((card :reader card
         :initarg :card)
   (deck-manager :reader deck-manager
                 :initarg :deck-manager)))

(defmethod discard-card ((deck-manager deck-manager) card)
  (when (not (member card (remaining-deck deck-manager)))
    (error 'card-not-in-deck :card card :deck-manager deck-manager))
  (setf (remaining-deck deck-manager)
        (remove card (remaining-deck deck-manager) :count 1))
  (push card (discard-pile deck-manager)))


(in-package :rush)

(define-condition selector-full (error)
  ())

(define-condition invalid-selection (error)
  ())

(defclass selector ()
  ((valid-options :reader valid-options
                  :initarg :valid-options)
   (selections :reader selections
               :writer initialize-selections)))

(defmethod initialize-instance :after ((selector selector) &key selection-count)
  (initialize-selections (make-array selection-count :fill-pointer 0)
                         selector))

(defmethod selector-fullp ((selector selector))
  (eql (length (selections selector))
       (array-dimension (selections selector) 0)))

(defmethod select ((selector selector) item)
  (when (selector-fullp selector)
    (error 'selector-full))
  (unless (member item (valid-options selector))
    (error 'invalid-selection))
  (vector-push item (selections selector)))

(defmethod selected-items ((selector selector))
  (coerce (selections selector) 'list))

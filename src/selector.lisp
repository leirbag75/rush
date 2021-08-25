
(in-package :rush)

(define-condition selector-full (error)
  ((selector :reader selector
             :initarg :selector)
   (item :reader item
         :initarg :item)))

(define-condition unselect-empty-selector (error)
  ((selector :reader selector
             :initarg :selector)))

(define-condition invalid-selection (error)
  ((selector :reader selector
             :initarg :selector)
   (item :reader item
         :initarg :item)))

(defclass selector ()
  ((valid-options :reader %valid-options
                  :initarg :valid-options)
   (selections :reader selections
               :writer initialize-selections)
   (test :reader test
         :initarg :test)
   (key :reader key
        :initarg :key))
  (:default-initargs :test #'eql :key #'identity))

(defmethod initialize-instance :after ((selector selector) &key selection-count)
  (initialize-selections (make-array selection-count :fill-pointer 0)
                         selector))

(defmethod valid-options ((selector selector))
  (remove-if (lambda (option)
               (member option
                       (selected-items selector)
                       :test (test selector)
                       :key (key selector)))
             (%valid-options selector)))

(defmethod selector-fullp ((selector selector))
  (zerop (- (array-dimension (selections selector) 0)
            (length (selections selector)))))

(defmethod select ((selector selector) item)
  (when (selector-fullp selector)
    (error 'selector-full :selector selector :item item))
  (unless (member (funcall (key selector) item)
                  (valid-options selector)
                  :test (test selector))
    (error 'invalid-selection :selector selector :item item))
  (vector-push item (selections selector)))

(defmethod unselect ((selector selector))
  (when (zerop (length (selections selector)))
    (error 'unselect-empty-selector :selector selector))
  (vector-pop (selections selector)))

(defmethod selected-items ((selector selector))
  (coerce (selections selector) 'list))

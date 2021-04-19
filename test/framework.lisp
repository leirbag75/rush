
(in-package :rush)

(defvar *suites* (make-hash-table))

(defvar *current-test* nil)

(defun tests (suite)
  (values (gethash suite *suites*)))

(defun (setf tests) (list suite)
  (setf (gethash suite *suites*)
        list))

(defun add-test (test suite)
  (pushnew test (tests suite))
  test)

(defun remove-test (test suite)
  (setf (tests suite)
        (remove test (tests suite)))
  test)

(defun call-with-test-restarts (function)
  (tagbody
   retry-test
     (restart-case (funcall function)
       (retry-test ()
         :report "Retry the test"
         (go retry-test))
       (end-test ()
         :report "Call current test a failure and move on"
         (return-from call-with-test-restarts)))))

(defmacro with-test-restarts (&body body)
  `(call-with-test-restarts (lambda () ,@body)))

(defvar *test-output* *standard-output*)

(defun test-output-prefix ()
  (format nil
          "~@[In ~S: ~]"
          *current-test*))

(defun log-test-event (string)
  (format *test-output*
          "~2&~A~A"
          (test-output-prefix)
          string))

(defun log-and-continue (error)
  (log-test-event (format nil "~A" error))
  (let ((restart (find-restart 'end-test)))
    (when restart
      (invoke-restart restart))))

(defun leave-to-debugger (error)
  (declare (ignore error)))

(defun run-tests (suite &key debug-on-failure)
  (handler-bind ((error (if debug-on-failure
                            #'leave-to-debugger
                            #'log-and-continue)))
    (mapc #'call-with-test-restarts (tests suite))
    (values)))

(defun run-all-tests (&key debug-on-failure)
  (maphash (lambda (suite value)
             (declare (ignore value))
             (handler-bind ((error (if debug-on-failure
                                       #'leave-to-debugger
                                       #'log-and-continue)))
               (mapc #'call-with-test-restarts (tests suite))
               (values)))
           *suites*)
  (values))

(defmacro deftest (name (suite) &body body)
  `(add-test (defun ,name ()
               (let ((*current-test* ',name))
                 ,@body
                 (values)))
             ',suite))

(defmacro weird-lambda ((marker) &body body)
  (let ((closure-var (gensym "CLOSURE")))
    `(lambda (,closure-var)
       (symbol-macrolet ((,marker (funcall ,closure-var)))
         ,@body))))

(defun call-with-expected-evaluations (weird-lambda on-failure
                                       &key (times 1) value)
  (let ((times-evaluated 0))
    (funcall weird-lambda
             (lambda () (incf times-evaluated) value))
    (unless (eql times-evaluated times)
      (funcall on-failure))))

(defmacro should-be-evaluated ((marker &rest args &key (times 1) value
                                &allow-other-keys)
                               (&rest error-args)
                               &body body)
  (declare (ignore times value))
  `(call-with-expected-evaluations (weird-lambda (,marker)
                                     ,@body)
                                   (lambda () (error ,@error-args))
                                   ,@args))

(defun call-with-expected-condition (condition-type function on-failure)
  (handler-bind ((t (lambda (condition)
                      (when (typep condition condition-type)
                        (return-from call-with-expected-condition)))))
    (funcall function))
  (funcall on-failure))

(defmacro should-signal ((condition) (&rest error-args) &body body)
  `(call-with-expected-condition ',condition
                                 (lambda () ,@body)
                                 (lambda () (error ,@error-args))))

(define-condition objects-dont-match (error)
  ((expected :reader expected
             :initarg :expected)
   (actual :reader actual
           :initarg :actual))
  (:report (lambda (condition stream)
             (format stream
                     "Failed assertion
Expected value: ~A
Got value: ~A"
                     (expected condition)
                     (actual condition)))))

(defun lists-match-p (element-predicate list1 list2)
  (and (eql (length list1)
            (length list2))
       (every element-predicate list1 list2)))

(defun all-events-match-p (expected actual)
  (lists-match-p #'events-match-p expected actual))

(defun assert-events-match (event-accumulator &rest expected)
  (let ((actual (next-events event-accumulator)))
    (unless (all-events-match-p expected actual)
      (error 'objects-dont-match
             :expected expected
             :actual actual))))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (error 'objects-dont-match
           :expected expected
           :actual actual)))

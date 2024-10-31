(defun bubble-pass (lst flag)
  (if (null (cdr lst)) 
      (values lst flag) 
      (let ((first (car lst))
            (second (cadr lst)))
        (if (> first second) 
            (multiple-value-bind (rest new-flag) (bubble-pass (cons first (cddr lst)) t)
              (values (cons second rest) new-flag)) 
            (multiple-value-bind (rest new-flag) (bubble-pass (cdr lst) flag)
              (values (cons first rest) new-flag)))))) 

(defun bubble-sort (lst)
  (let ((sorted lst))
    (if (or (null lst) (null (cdr lst))) 
        sorted
        (multiple-value-bind (new-sorted flag) (bubble-pass sorted nil) 
          (if (not flag) 
              new-sorted
              (bubble-sort new-sorted)))))) 

(defun check-sort (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (bubble-sort input) expected) 
          name))

(defun test-sorting-functions ()
  (check-sort "Functional test 1" '(3 1 2) '(1 2 3))
  (check-sort "Functional test 2" '(5 3 4 2 1) '(1 2 3 4 5))
  (check-sort "Functional test 3" '(1 2 3) '(1 2 3))
  (check-sort "Functional test 4" '(5 5 5) '(5 5 5)))

(test-sorting-functions)

(defun exchange2 (A)
  (let ((R (1- (length A)))
        (flag t))
    (loop while flag do
      (setf flag nil)
      (dotimes (i R)
        (when (> (nth i A) (nth (1+ i) A))
          (let ((tmp (nth i A)))
            (setf (nth i A) (nth (1+ i) A))
            (setf (nth (1+ i) A) tmp)
            (setf flag t))))
      (decf R))
    A))


(defun check-sort-imp (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (exchange2 input) expected) 
          name))

(defun test-sorting-functions-imp ()
  ;; Тести для функціонального варіанту
  (check-sort-imp "Functional test 1" '(3 1 2) '(1 2 3))
  (check-sort-imp "Functional test 2" '(5 3 4 2 1) '(1 2 3 4 5))
  (check-sort-imp "Functional test 3" '(1 2 3) '(1 2 3))
  (check-sort-imp "Functional test 4" '(5 5 5) '(5 5 5)))

(test-sorting-functions-imp)

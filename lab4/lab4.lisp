;Перша частина

(defun bubble-pass (lst flag key test)
  (if (null (cdr lst))
      (values lst flag)
      (let ((first (car lst))
            (second (cadr lst)))
        (if (funcall test (funcall key first) (funcall key second)) 
            (multiple-value-bind (rest new-flag) (bubble-pass (cons first (cddr lst)) t key test)
              (values (cons second rest) new-flag)) 
            (multiple-value-bind (rest new-flag) (bubble-pass (cdr lst) flag key test)
              (values (cons first rest) new-flag))))))

(defun bubble-sort (lst &key (key #'identity) (test #'>))
  (let ((sorted lst))
    (if (or (null lst) (null (cdr lst))) 
        sorted
        (multiple-value-bind (new-sorted flag) (bubble-pass sorted nil key test) 
          (if (not flag) 
              new-sorted
              (bubble-sort new-sorted :key key :test test))))))

(defun check-sort (name input expected &key (key #'identity) (test #'>))
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (bubble-sort input :key key :test test) expected) 
          name))

(defun test-sorting-functions ()
  
  (check-sort "Functional test 1" '(3 1 2) '(1 2 3))
  
  (check-sort "Functional test 2" '(5 3 1 4 2 1) '(1 1 2 3 4 5))
  
  (check-sort "Functional test 3" '(3 1 -1 2 3 5 6) '(6 5 3 3 2 1 -1)  :test #'<)
  
  (check-sort "Functional test 4" '(7 12 3 19 5) '(19 3 7 12 5) :key (lambda (x) (mod x 5)) :test #'<)  
  
  (check-sort "Functional test 5" '(-3 -1 2 5 -4) '(-1 2 -3 -4 5) :key #'abs)) 
  
;Друга частина

(defun duplicate-elements-fn (n &key duplicate-p)
  (lambda (x)
    (if (or (not duplicate-p) (funcall duplicate-p x))
        (make-list n :initial-element x)
        (list x))))

(defun check-duplicate-elements-fn (name input  expected n  &key duplicate-p )
  
  (let ((result (mapcan (duplicate-elements-fn n :duplicate-p duplicate-p) input)))
    (format t "~:[FAILED~;PASSED~]... ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "Expected: ~a~%Got: ~a~%~%" expected result))))

(defun test-duplicate-elements-fn ()
  (check-duplicate-elements-fn "Functional test 1" '() '() 2)
  (check-duplicate-elements-fn "Functional test 2" '(1 2 3) '(1 2 3) 1)
  (check-duplicate-elements-fn "Functional test 3" '(1 2 3) '(1 1 2 2 3 3) 2)
  (check-duplicate-elements-fn "Functional test 4" '(1 2 3 4) '(1 2 2 2 3 4 4 4) 3 :duplicate-p #'evenp)
  (check-duplicate-elements-fn "Functional test 5" '(1 2 3 4) '(1 2 3 3 4 4) 2 :duplicate-p (lambda (x) (> x 2))))


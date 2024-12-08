(defun merge-lists-spinning-pairs (a b)
  (cond
    ((null a) 
     (if (null b) nil (cons (list (car b)) (merge-lists-spinning-pairs nil (cdr b)))))
    ((null b) 
     (cons (list (car a)) (merge-lists-spinning-pairs (cdr a) nil)))
    (t 
     (cons (list (car a) (car b)) (merge-lists-spinning-pairs (cdr b) (cdr a))))))

(defun check-merge-lists-spinning-pairs (name a b expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
    (equal (merge-lists-spinning-pairs a b) expected)
    name)
)

(check-merge-lists-spinning-pairs "Task 1 Test 1" '(1 2 3) '(a b c) '((1 A) (B 2) (3 C)))
(check-merge-lists-spinning-pairs "Task 1 Test 2" '(1 2 3 4 5) '(a b c d) '((1 A) (B 2) (3 C) (D 4) (5)))
(check-merge-lists-spinning-pairs "Task 1 Test 3" '(1 2) '(a b c d e) '((1 A) (B 2) (C) (D) (E)))
(check-merge-lists-spinning-pairs "Task 1 Test 4" '() '(a b c d) '((A) (B) (C) (D)))
(check-merge-lists-spinning-pairs "Task 1 Test 5" '(1 2 3 4) '() '((1) (2) (3) (4)))

(defun list-set-intersect-p (a b)
  (unless (null a)
    (let ((x (car a)))
      (if (find-in-list x b)
          t
        (list-set-intersect-p (cdr a) b)))))

(defun find-in-list (x lst)
  (cond ((null lst) nil)
        ((eq x (car lst)) t)
        (t (find-in-list x (cdr lst)))))

(defun check-list-set-intersect-p (name a b expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
    (equal (list-set-intersect-p a b) expected)
    name)
)

(check-list-set-intersect-p "Task 2 Test 1" '(4 5 3 8) '(4 5 6 7) T)
(check-list-set-intersect-p "Task 2 Test 2" '(2 3 4 5) '(5 6 7 8) T)
(check-list-set-intersect-p "Task 2 Test 3" '(6 7 8 4) '(4 8 5 4) T)
(check-list-set-intersect-p "Task 2 Test 4" '(8 3 4 2) '(9 5 6 7) NIL)
(check-list-set-intersect-p "Task 2 Test 5" '(A B C D) '(C D E F) T)

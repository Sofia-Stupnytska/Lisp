(defvar  my-list (setq my-list (cons `a (list 1 (list `b `c) `() 5))) )
(print  my-list)
(print  (car my-list))
(print  (cdr my-list))
(print  (third my-list))
(print  (car (last my-list) ) )
(print  (atom (car my-list) ) )
(print  (atom (cdr my-list) ) )
(print  (listp (car my-list)) )
(print  (listp (cdr my-list)) )
(print  (eq (car my-list) (third my-list)) )
(print  (null (car my-list)) )
(print  (append my-list (third my-list)))

(let ((x (list 4 'E 'F)))
(print (list 'D x '(5) (car (last x))))
)
;part 1
;item 1
(defvar  my-list (setq my-list (cons 'a (list 1 (list 'b 'c) '() 5))) )
(print  my-list)
;item 2
(print  (car my-list))
;item 3
(print  (cdr my-list))
;item 4
(print  (third my-list))
;item 5
(print  (car (last my-list) ) )
;item 6
(print  (atom (car my-list) ) )
(print  (atom (cdr my-list) ) )
(print  (listp (car my-list)) )
(print  (listp (cdr my-list)) )
;item 7
(print  (eq (car my-list) (third my-list)) )
(print  (null (car my-list)) )
;item 8
(print  (append my-list (third my-list)))

;part 2
(let ((x (list 4 'E 'F)))
(print (list* 'D x '(5) (last x)))
)

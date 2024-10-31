<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Ступницька Софія Миколаївна КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.
## Варіант 5
Алгоритм сортування обміном No2 (із використанням прапорця) за незменшенням.
## Лістинг функції з використанням конструктивного підходу
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
passed... Functional test 1
passed... Functional test 2
passed... Functional test 3
passed... Functional test 4
```
## Лістинг функції з використанням деструктивного підходу
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
passed... Functional test 1
passed... Functional test 2
passed... Functional test 3
passed... Functional test 4
```

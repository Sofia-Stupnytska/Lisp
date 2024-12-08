<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Ступницька Софія Миколаївна  КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за можливості/необхідності використовуючи різні види рекурсії. 
Функції, які необхідно реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій для роботи зі списками, що не наведені в четвертому розділі навчального посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів. Кожна реалізована функція має бути протестована для різних тестових наборів. Тести мають бути оформленні у вигляді модульних тестів (див. п. 2.3). Додатковий бал за лабораторну роботу можна отримати в разі виконання всіх наступних умов:
робота виконана до дедлайну (включно з датою дедлайну)
крім основних реалізацій функцій за варіантом, також реалізовано додатковий варіант однієї чи обох функцій, який працюватиме швидше за основну реалізацію, не порушуючи при цьому перші три вимоги до основної реалізації (вимоги 4 і 5 можуть бути порушені), за виключенням того, що в разі необхідності можна також використати стандартну функцію copy-list
## Варіант 6
1. Написати функцію merge-lists-spinning-pairs , яка групує відповідні елементи
двох списків, почергово змінюючи їх взаємне розташування в групі:
```lisp
CL-USER> (merge-lists-spinning-pairs '(1 2 3 4 5) '(a b c d))
((1 A) (B 2) (3 C) (D 4) (5))
```
3. Написати предикат list-set-intersect-p , який визначає чи перетинаються дві
множини, задані списками атомів, чи ні:
```lisp
CL-USER> (list-set-intersect-p '(1 2 3) '(4 5 6))
NIL
CL-USER> (list-set-intersect-p '(1 2 3) '(3 4 5))
T
```
## Лістинг функції merge-lists-spinning-pairs
```lisp
(defun merge-lists-spinning-pairs (a b)
  (cond
    ((null a) 
     (if (null b) nil (cons (list (car b)) (merge-lists-spinning-pairs nil (cdr b)))))
    ((null b) 
     (cons (list (car a)) (merge-lists-spinning-pairs (cdr a) nil)))
    (t 
     (cons (list (car a) (car b)) (merge-lists-spinning-pairs (cdr b) (cdr a))))))
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
passed... Task 1 Test 1
passed... Task 1 Test 2
passed... Task 1 Test 3
passed... Task 1 Test 4
passed... Task 1 Test 5
```
## Лістинг функції list-set-intersect-p
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
passed... Task 2 Test 1
passed... Task 2 Test 2
passed... Task 2 Test 3
passed... Task 2 Test 4
passed... Task 2 Test 5
```

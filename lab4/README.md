<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студентка</b>: 
 Ступницька Софія Миколаївна КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Завдання складається з двох частин:

1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
   роботи 3 з такими змінами:

- використати функції вищого порядку для роботи з послідовностями (де це
  доречно);
- додати до інтерфейсу функції (та використання в реалізації) два ключових
  параметра: key та test , що працюють аналогічно до того, як працюють
  параметри з такими назвами в функціях, що працюють з послідовностями. При
  цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
   варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
   можливості, має бути мінімізоване.

## Варіант першої частини (варіант 5)

Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
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
```

### Тестові набори та утиліти першої частини

```lisp
(defun check-sort (name input expected &key (key #'identity) (test #'>))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (bubble-sort input :key key :test test) expected)
          name))

(defun test-sorting-functions ()
  ;Звичайні тести
  (check-sort "Functional test 1" '(3 1 2) '(1 2 3))

  (check-sort "Functional test 2" '(5 3 1 4 2 1) '(1 1 2 3 4 5))

  (check-sort "Functional test 3" '(3 1 -1 2 3 5 6) '(6 5 3 3 2 1 -1)  :test #'<)

  (check-sort "Functional test 4" '(7 12 3 19 5) '(19 3 7 12 5) :key (lambda (x) (mod x 5)) :test #'<)

  (check-sort "Functional test 5" '(-3 -1 2 5 -4) '(-1 2 -3 -4 5) :key #'abs))
```

### Тестування першої частини

```lisp
CL-USER> (test-sorting-functions)
passed... Functional test 1
passed... Functional test 2
passed... Functional test 3
passed... Functional test 4
passed... Functional test 5
NIL
```

## Варіант другої частини 9

Написати функцію **duplicate-elements-fn** , яка має один основний параметр n та
один ключовий параметр — функцію duplicate-p . duplicate-elements-fn має
повернути функцію, яка при застосуванні в якості першого аргументу mapcan робить
наступне: кожен елемент списка-аргумента mapcan , для якого функція duplicate-p
повертає значення t (або не nil ), дублюється n разів. Якщо користувач не передав
функцію duplicate-p у duplicate-elements-fn , тоді дублюються всі елементи вхідного
списку.

```lisp
CL-USER> (mapcan (duplicate-elements-fn 2) '(1 2 3))
(1 1 2 2 3 3)
CL-USER> (mapcan (duplicate-elements-fn 2 :duplicate-p #'evenp) '(1 2 3 4 5))
(1 2 2 3 4 4 5)
```

## Лістинг реалізованої програми

```lisp
(defun duplicate-elements-fn (n &key (duplicate-p #'identity))
  (lambda (x)
    (if (funcall duplicate-p x)
        (make-list n :initial-element x)
        (list x))))
```

### Тестові набори та утиліти

```lisp
(defun check-duplicate-elements-fn (name input  expected n  &key  (duplicate-p #'identity) )

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
```

### Тестування

```lisp
CL-USER> ( test-duplicate-elements-fn)
PASSED... Functional test 1
PASSED... Functional test 2
PASSED... Functional test 3
PASSED... Functional test 4
PASSED... Functional test 5
NIL
```

<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студентка</b>: 
 Ступницька Софія Миколаївна КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту): структури у геш-таблиці, геш-таблиці у асоціативні списки, асоціативні списки у геш-таблиці.
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 9
База даних: Наукові статті
Тип записів: Асоціативний список
## Лістинг реалізації завдання
```lisp
;Визначення кількості та типів колонок таблиць
(defparameter *specialty* '(:number :string))
(defparameter *article* '(:number :string :string))

;Реалізація читання з csv і представлення у вигляді таблиці
(defun read-table-from-csv (path)
  (if (and path (stringp path))
      (with-open-file (stream path :if-does-not-exist :error)
        (loop with table = '()
              for line = (read-line stream nil nil)
              while line
              do (push (uiop:split-string (string-trim '(#\Return) line) :separator '(#\;)) table)
              finally (return (reverse table))))
      (error "Invalid path: ~A" path)))


;Реалізація перетворення таблиці на асоц. список
(defun table-to-alist (table)
  (mapcan (lambda (x) (list (reverse (pairlis (car table) x)))) (cdr table)))

;Реалізація прив'язки типів до елементів асоц. списку
(defun assign-types (elems types)
  (if (null elems)
      '()
      (let* ((key (intern (string-upcase (car (car elems))) :keyword))
             (value (case (car types)
                      (:number (read-from-string (cdr (car elems))))
                      (:string (cdr (car elems)))
                      (otherwise (error "Invalid keyword in conf-list: ~s.~%" (car types))))))
        (cons (cons key value)
              (assign-types (cdr elems) (cdr types))))))


;Реалізація функції select
(defun select (file-path column-config)
  "`column-config` must match the number of table columns.
    Accepts a list of keyword symbols, distinguishing only `:number` and `:string`; others are invalid."

  (let ((data-alist (table-to-alist (read-table-from-csv file-path))))
    (if (eql (length (car data-alist)) (length column-config))
        (let ((processed-alist (mapcan (lambda (row) (list (assign-types row column-config))) data-alist))
              (filtered-results '())
              (match-status t))
          (lambda (&rest filters)
            (if (evenp (length filters))
                (progn
                  (dolist (row processed-alist)
                    (loop for filter-args on filters by #'cddr
                          while (and filter-args match-status)
                          do
                          (unless (equal (cdr (assoc (first filter-args) row))
                                         (second filter-args))
                            (setf match-status nil)))
                    (if match-status
                        (push row filtered-results)
                        (setf match-status t)))
                  (reverse filtered-results))
                (error "Filters must have an even number of arguments.~% 
                        Each filter should consist of a keyword symbol followed by a value."))))
        
        (error "The length of `column-config` must match the number of columns in the table file.
                ~% Number of columns: ~a~% Length of `column-config`: ~a~%"
               (length (car data-alist)) (length column-config)))))

;Допоміжні функції для запису в файл
(defun is-empty-line (text)
  (if (string= text "")
      nil
      ";"))

(defun stringify (value)
  (if (stringp value)
      value
      (write-to-string value)))

;Реалізація запису в файл
(defun write-to-csv (path typed-alist)
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((header-line ""))
      (dolist (header (car typed-alist))
        (setf header-line (concatenate 'string header-line (is-empty-line header-line) (string-capitalize (string (car header))))))
      (format s "~a~%" header-line)
      
      (dolist (row typed-alist)
        (let ((data-line ""))
          (dolist (cell row)
            (setf data-line (concatenate 'string data-line (is-empty-line data-line) (stringify (cdr cell)))))
          (format s "~a~%" data-line))))))

;Реалізація перетворення асоц. списку в хеш таблицю
(defun alist-to-hash (alist)
  (let ((hash-tables '())
        (current-table nil))
    (dolist (entry alist)
      (setf current-table (make-hash-table :test 'eq))
      (dolist (key-value entry)
        (setf (gethash (car key-value) current-table) (cdr key-value)))
      (push current-table hash-tables))
    (reverse hash-tables)))

;Реалізація виводу асоц. списку
(defun print-alist (alist)
  (dolist (var (car alist))
    (format t "~15a " (string-capitalize (string (car var))))
    
    )
  (format t "~%")
  (dolist (var alist)
    (dolist (elem var)
      (format t "~15s " (cdr elem))
      )
    (format t "~%")
    )
  )

;Реалізація виводу асоц. списку
(defun print-hash-list (hash-list)
  (dolist (hash hash-list)
    (maphash (lambda (key value)
               (format t "~15a ~a~%" (string-capitalize (string key)) value))
             hash)
    (format t "---~%")))
```
### Тестові набори та утиліти
```lisp
(defun test-read-csv-and-print ()
  (let ((specialty-data (funcall (select "projects/lab5/specialty.csv" *specialty*)))
        (filtered-by-specialty (funcall (select "projects/lab5/article.csv" *article*) :specialty "F2"))
        (filtered-by-specialty-and-article (funcall (select "projects/lab5/article.csv" *article*) 
                                                    :specialty "F7" :article "Article7")))

    (format t "Results for specialties (all rows):~%")
    (print-alist specialty-data)
    (format t "~%")

    (format t "Results filtered by specialty = 'F2':~%")
    (print-alist filtered-by-specialty)
    (format t "~%")

    (format t "Results filtered by specialty = 'F7' and article = 'Article7':~%")
    (print-alist filtered-by-specialty-and-article)
    (format t "~%")))

(defun test-alist-to-hash-table ()
  (let* ((alist '(((:ID . 1) (:ARTICLE . "Article1") (:SPECIALTY . "F1"))
                  ((:ID . 2) (:ARTICLE . "Article2") (:SPECIALTY . "F2"))))
         (hash-list (alist-to-hash alist)))
    ;; Використовуємо print-hash-list для виводу
    (format t "Converted Hash Tables:~%")
    (print-hash-list hash-list)))

(defun test-write-to-csv ()
  (let* ((input-file "projects/lab5/article.csv")
         (output-file "projects/lab5/specialty_F7.csv")
         (conf-list *article*) ; Використовуємо змінну *article* як конфігурацію колонок
         (selector (select input-file conf-list))
         (filtered-alist (funcall selector :specialty "F7")))
    ;; Записуємо відібрані дані в новий CSV файл
    (write-to-csv output-file filtered-alist)
    (format t "Data where :specialty is 'F7' has been written to ~a~%" output-file)))

(test-read-csv-and-print)
(test-alist-to-hash-table)
(test-write-to-csv)
```
### Вміст тестових файлів

specialty.csv
```
ID	SPECIALTY
1	F1
2	F2
3	F3
4	F7
```

article.csv
```
ID	ARTICLE	SPECIALTY
1	Article1	F1
2	Article2	F2
3	Article3	F3
4	Article4	F3
5	Article5	F7
6	Article6	F7
7	Article7	F7
8	Article8	F7

```

### Тестування
```lisp
Results for specialties (all rows):
Id              Specialty
1               "F1"
2               "F2"
3               "F3"
4               "F7"

Results filtered by specialty = 'F2':
Id              Article         Specialty
2               "Article2"      "F2"

Results filtered by specialty = 'F7' and article = 'Article7':
Id              Article         Specialty
7               "Article7"      "F7"

Converted Hash Tables:
Id              1
Article         Article1
Specialty       F1
---
Id              2
Article         Article2
Specialty       F2
---
Data where :specialty is 'F7' has been written to specialty_F7.csv
```
## Результат записання в specialty_F7.csv файл:
Id	Article	Specialty
5	Article5	F7
6	Article6	F7
7	Article7	F7
8	Article8	F7




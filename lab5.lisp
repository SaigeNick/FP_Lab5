;; ----1. Оголошення структур----------------------------------
(defstruct manufacturer
  id
  name
  country
  founded-year
  specialization)

(defstruct drone
  id
  manufacturer-id
  model
  weight
  range)

;; ----2. split-string-----------------------------------------
(defun split-string (string separator)
  "Розбиває рядок STRING на підрядки за роздільником SEPARATOR."
  (loop with result = nil
        for start = 0 then (1+ end)
        for end = (position separator string :start start)
        while end
        do (push (subseq string start end) result)
        finally (push (subseq string start) result)
        (return (nreverse result))))

;; ----3. parse-float (справді парсить дробове число)----------
(defun parse-float (string)
  "Перетворює рядок STRING у дійсне число (або NIL, якщо це не можливо)."
  (ignore-errors
    (let ((val (read-from-string string nil nil)))
      (cond
        ((floatp val) val)
        ((integerp val) (float val))
        (t nil)))))

;; ----4. Зчитування таблиць з файлу---------------------------
(defun read-csv-to-structures (filename structure-type)
  "Читає CSV файл та повертає список структур заданого типу."
  (with-open-file (stream filename :direction :input)
    ;; Спершу зчитаємо рядок заголовків, щоб пропустити його
    (let ((headers (split-string (read-line stream) #\,)))
      (loop for line = (read-line stream nil nil)
            while line
            collect (let ((values (split-string line #\,)))
                      (case structure-type
                        (manufacturer
                         (make-manufacturer
                          :id (parse-integer (nth 0 values) :junk-allowed t)
                          :name (nth 1 values)
                          :country (nth 2 values)
                          :founded-year (parse-integer (nth 3 values) :junk-allowed t)
                          :specialization (nth 4 values)))
                        (drone
                         (make-drone
                          :id (parse-integer (nth 0 values) :junk-allowed t)
                          :manufacturer-id (parse-integer (nth 1 values) :junk-allowed t)
                          :model (nth 2 values)
                          :weight (parse-float (nth 3 values))   ;; тепер справді float
                          :range (parse-integer (nth 4 values) :junk-allowed t)))))))))

;; ----5. Функція select з лямбдою, що приймає фільтри---------
(defun select (filename structure-type)
  "Повертає лямбда-вираз, який при виклику з ключами фільтрує записи."
  (let ((all-records (read-csv-to-structures filename structure-type)))
    (lambda (&rest filters)
      (if (null filters)
          ;; Якщо фільтри не задано, повертаємо всі записи
          all-records
          (let ((filter-pairs
                 (loop for (key value) on filters by #'cddr
                       collect (cons key value))))
            (remove-if-not
             (lambda (record)
               (every
                (lambda (filter)
                  (let* ((field-name (car filter))
                         (expected (cdr filter))
                         (actual
                          (ecase structure-type
                            (manufacturer
                             (case field-name
                               (:id (manufacturer-id record))
                               (:name (manufacturer-name record))
                               (:country (manufacturer-country record))
                               (:founded-year (manufacturer-founded-year record))
                               (:specialization (manufacturer-specialization record))))
                            (drone
                             (case field-name
                               (:id (drone-id record))
                               (:manufacturer-id (drone-manufacturer-id record))
                               (:model (drone-model record))
                               (:weight (drone-weight record))
                               (:range (drone-range record)))))))
                    (equal actual expected)))
                filter-pairs))
             all-records))))))

;; ----6. Запис вибірки у файл----------------------------------
(defun write-selection-to-csv (records filename structure-type)
  "Записує список записів у CSV-файл."
  (with-open-file (stream filename :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
    (ecase structure-type
      (manufacturer
       (format stream "id,name,country,founded-year,specialization~%"))
      (drone
       (format stream "id,manufacturer-id,model,weight,range~%")))
    (dolist (record records)
      (ecase structure-type
        (manufacturer
         (format stream "~A,~A,~A,~A,~A~%"
                 (manufacturer-id record)
                 (manufacturer-name record)
                 (manufacturer-country record)
                 (manufacturer-founded-year record)
                 (manufacturer-specialization record)))
        (drone
         (format stream "~A,~A,~A,~A,~A~%"
                 (drone-id record)
                 (drone-manufacturer-id record)
                 (drone-model record)
                 (drone-weight record)
                 (drone-range record)))))))

;; ----7. "Красивий" вивід--------------------------------------
(defun pretty-print-records (records structure-type)
  "Виводить список записів у більш читабельному форматі."
  (dolist (record records)
    (ecase structure-type
      (manufacturer
       (format t "~10A: ~A~%~10A: ~A~%~10A: ~A~%~10A: ~A~%~10A: ~A~%~%"
               "ID" (manufacturer-id record)
               "Name" (manufacturer-name record)
               "Country" (manufacturer-country record)
               "Founded Year" (manufacturer-founded-year record)
               "Specialization" (manufacturer-specialization record)))
      (drone
       (format t "~10A: ~A~%~10A: ~A~%~10A: ~A~%~10A: ~A~%~10A: ~A~%~%"
               "ID" (drone-id record)
               "Manufacturer ID" (drone-manufacturer-id record)
               "Model" (drone-model record)
               "Weight" (drone-weight record)
               "Range" (drone-range record))))))

;; ----8. Функції конвертації------------------------------------
(defun structure-to-hash-table (structure)
  "Конвертує структуру у геш-таблицю."
  (let ((hash (make-hash-table :test 'equal)))
    (cond
      ((typep structure 'manufacturer)
       (setf (gethash 'id hash) (manufacturer-id structure))
       (setf (gethash 'name hash) (manufacturer-name structure))
       (setf (gethash 'country hash) (manufacturer-country structure))
       (setf (gethash 'founded-year hash) (manufacturer-founded-year structure))
       (setf (gethash 'specialization hash) (manufacturer-specialization structure)))
      ((typep structure 'drone)
       (setf (gethash 'id hash) (drone-id structure))
       (setf (gethash 'manufacturer-id hash) (drone-manufacturer-id structure))
       (setf (gethash 'model hash) (drone-model structure))
       (setf (gethash 'weight hash) (drone-weight structure))
       (setf (gethash 'range hash) (drone-range structure))))
    hash))

(defun hash-table-to-alist (hash)
  "Конвертує геш-таблицю у асоціативний список."
  (loop for key being the hash-keys of hash
        collect (cons key (gethash key hash))))

(defun alist-to-hash-table (alist)
  "Конвертує асоціативний список у геш-таблицю."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

;; ----9. Тести (приклади використання)--------------------------
(defun test-read-display-data ()
  (let ((manufacturers-fn (select "~/LISP/manufacturers.csv" 'manufacturer))
        (drones-fn (select "~/LISP/drones.csv" 'drone)))
    (format t "Manufacturers data:~%")
    (pretty-print-records (funcall manufacturers-fn) 'manufacturer)
    (format t "~%Drones data:~%")
    (pretty-print-records (funcall drones-fn) 'drone)))

(defun test-filter-data ()
  (let ((usa-manufacturers-fn (select "~/LISP/manufacturers.csv" 'manufacturer))
        (long-range-drones-fn (select "~/LISP/drones.csv" 'drone)))
    ;; Тепер фільтрація при виклику лямбди, а не при створенні
    (format t "USA Manufacturers:~%")
    (pretty-print-records
     (funcall usa-manufacturers-fn :country "USA") 'manufacturer)

    (format t "~%Long-range drones (range=40):~%")
    (pretty-print-records
     (funcall long-range-drones-fn :range 40) 'drone)))

(defun test-data-transformation ()
  (let* ((drones-fn (select "~/LISP/drones.csv" 'drone))
         (drone (first (funcall drones-fn)))
         (hash (structure-to-hash-table drone))
         (alist (hash-table-to-alist hash))
         (hash-again (alist-to-hash-table alist)))
    (format t "Drone as hash table:~%")
    (maphash (lambda (key value)
               (format t "~a => ~a~%" key value))
             hash)
    (format t "~%Drone as alist:~%")
    (dolist (pair alist)
      (format t "~a => ~a~%" (car pair) (cdr pair)))
    (format t "~%Drone back to hash table:~%")
    (maphash (lambda (key value)
               (format t "~a => ~a~%" key value))
             hash-again)))

(defun test-add-drone ()
  (let ((data (list (make-drone
                     :id 106
                     :manufacturer-id 1
                     :model "Inspire 2"
                     :weight 3.44
                     :range 27))))
    (write-selection-to-csv data "~/LISP/new-drones.csv" 'drone)))

(defun test-add-manufacturer ()
  (let ((data (list (make-manufacturer
                     :id 5
                     :name "SkyMakers"
                     :country "Canada"
                     :founded-year 2020
                     :specialization "Advanced Research"))))
    (write-selection-to-csv data "~/LISP/new-manufacturers.csv" 'manufacturer)))

(defun run-all-tests ()
  (test-read-display-data)
  (test-filter-data)
  (test-data-transformation)
  (test-add-drone)
  (test-add-manufacturer))

;; Виклик усіх тестів
(run-all-tests)

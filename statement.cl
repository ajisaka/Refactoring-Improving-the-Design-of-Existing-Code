
(ql:quickload :cl-json)
(ql:quickload :uiop)
; (ql:quickload :alexandria)


(defparameter
  plays
  (json:decode-json-from-string
    (uiop:read-file-string "plays.json")))

(defparameter
  invoices
  (json:decode-json-from-string
    (uiop:read-file-string "invoices.json")))


(defun as-keyword (v)
  (intern (string-upcase v) :keyword))

(defun assoc-v (key alist)
  (cdr (assoc key alist)))




(defun statement (invoice plays)
  (labels ((amount-for
             (perf)
             (let* ((result 0)
                    (type (as-keyword (assoc-v :type (play-for perf))))
                    (audience (assoc-v :audience perf)))
               (case
                 type
                 (:TRAGEDY
                   (setf result 40000)
                   (when (< 30 audience)
                     (incf result
                           (* 1000 (- audience 30)))))
                 (:COMEDY
                   (setf result 30000)
                   (when (< 20 audience)
                     (incf result
                           (+ 10000
                              (* 500 (- audience 20)))))
                   (incf result (* 300 audience))))
               result))
           (play-for (perf)
                     (assoc-v
                       (as-keyword
                         (assoc-v
                           :play-id
                           perf))
                       plays))
           (volume-credits-for
             (perf)
             (let ((result 0)
                   (audience (assoc-v :audience perf)))
               (incf result (max (- audience 30) 0))
               (when (eq :COMEDY (as-keyword
                                   (assoc-v :type (play-for perf))))
                 (incf result
                       (floor (/ audience 5))))
               result))
           (total-volume-credits
             ()
             (let ((result 0))
               (loop for perf in (assoc-v :performances invoice)
                     do (incf result (volume-credits-for perf)))
               result))
           (total-amount
             ()
             (let ((result 0))
               (loop for perf in (assoc-v :performances invoice)
                     do (incf result (amount-for perf)))
               result)))
    (let ((result (format nil "Statement for ~A~%" (assoc-v :customer invoice))))
      (loop for perf in (assoc-v :performances invoice)
            do (setf result
                     (concatenate
                       'string
                       result
                       (format nil "    ~A: $~$ (~A seats)~%"
                               (assoc-v :name (play-for perf))
                               (/ (amount-for perf) 100)
                               (assoc-v :audience perf)))))
      (setf result
            (concatenate
              'string
              result
              (format nil "Amount owed is $~$~%" (/ (total-amount) 100))
              (format nil "You earned $~$~%" (total-volume-credits))))
      result)))

(loop for invoice in invoices
      do (format t "~A~%" (statement invoice plays)))

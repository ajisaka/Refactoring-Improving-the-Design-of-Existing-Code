
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


(defstruct statement-data
  customer
  performances
  total-amount
  total-volume-credits)

(defstruct performance
  play
  amount
  audience)


(defun render-plain-text (statement-data plays)
  (labels ()
    (let ((result (format nil "Statement for ~A~%" (statement-data-customer statement-data))))
      (loop for perf in (statement-data-performances statement-data)
            do (setf result
                     (concatenate
                       'string
                       result
                       (format nil "    ~A: $~$ (~A seats)~%"
                               (assoc-v :name (performance-play perf))
                               (/ (performance-amount perf) 100)
                               (performance-audience perf)))))
      (setf result
            (concatenate
              'string
              result
              (format nil "Amount owed is $~$~%" (/ (statement-data-total-amount statement-data) 100))
              (format nil "You earned $~$~%" (statement-data-total-volume-credits statement-data))))
      result)))

(defun Statement (invoice plays)
  (labels
    ((play-for (perf)
               (assoc-v
                 (as-keyword
                   (assoc-v
                     :play-id
                     perf))
                 plays))
     (amount-for
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
         result)))
    (let ((statement-data (make-statement-data
                            :customer (assoc-v :customer invoice)
                            :performances (mapcar
                                            (lambda (perf)
                                              (make-performance
                                                :play (play-for perf)
                                                :amount (amount-for perf)
                                                :audience (assoc-v :audience perf)))
                                            (assoc-v :performances invoice)))))
      (labels
        ((volume-credits-for
           (perf)
           (let ((result 0)
                 (audience (performance-audience perf)))
             (incf result (max (- audience 30) 0))
             (when (eq :COMEDY (as-keyword
                                 (assoc-v :type (performance-play perf))))
               (incf result
                     (floor (/ audience 5))))
             result))
         (total-amount
           (statement-data)
           (let ((result 0))
             (loop for perf in (statement-data-performances statement-data)
                   do (incf result (performance-amount perf)))
             result))
         (total-volume-credits
           (statement-data)
           (let ((result 0))
             (loop for perf in (statement-data-performances statement-data)
                   do (incf result (volume-credits-for perf)))
             result))
         )
        (setf (statement-data-total-amount statement-data)
              (total-amount statement-data))
        (setf (statement-data-total-volume-credits statement-data)
              (total-volume-credits statement-data))
        (render-plain-text statement-data plays)))))


(loop for invoice in invoices
      do (format t "~A~%" (statement invoice plays)))

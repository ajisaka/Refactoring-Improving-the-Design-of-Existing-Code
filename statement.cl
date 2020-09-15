
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
  (let ((total-amount 0)
        (volume-credits 0)
        (result (format nil "Statement for ~A~%" (assoc-v :customer invoice))))
    (loop for perf in (assoc-v :performances invoice)
          do (let* ((play-id (as-keyword (assoc-v :play-id perf)))
                    (play (assoc-v play-id plays))
                    (audience (assoc-v :audience perf))
                    (this-amount 0)
                    (type (as-keyword (assoc-v :type play))))
               (case
                 type
                 (:TRAGEDY
                   (setf this-amount 40000)
                   (when (< 30 audience)
                     (incf this-amount
                           (* 1000 (- audience 30)))))
                 (:COMEDY
                   (setf this-amount 30000)
                   (when (< 20 audience)
                     (incf this-amount
                           (+ 10000
                              (* 500 (- audience 20)))))
                   (incf this-amount (* 300 audience))))
               (incf volume-credits (max (- audience 30) 0))
               (when (eq :COMEDY type)
                 (incf volume-credits
                       (floor (/ audience 5))))
               (setf result
                     (concatenate
                       'string
                       result
                       (format nil "    ~A: $~$ (~A seats)~%"
                               (assoc-v :name play)
                               (/ this-amount 100)
                               audience)))
               (incf total-amount this-amount)))
    (setf result
          (concatenate
            'string
            result
            (format nil "Amount owed is ~$~%" (/ total-amount 100))
            (format nil "You earned ~$~%" volume-credits)))
    result))

(loop for invoice in invoices
      do (format t "~A~%" (statement invoice plays)))

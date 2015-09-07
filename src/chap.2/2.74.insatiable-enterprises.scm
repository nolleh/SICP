; 2.74.InsatiableEnterprises
; a. get-record
; the department must contain "key procedure for identify employee from whose name"
; and there isn't any constranit for record is "list", so also need iterating interface.
(define (get-record k-name records)
  (cond ((null? records) false)
        ((eq? k-name (key (head records)))
          (head records))
        (else (get-record k-name (head records)))))

; b. get-salary
; for identifying salary from un-constraints employee record, the department need to provide 
; selector for salary

(define (get-salary k-name records)
  (cond ((null? records) false)
        ((eq? k-name (key (head records)))
          (salary (head records)))
        (else (get-record k-name (head records)))))


; c. find-employee-record. files: list
(define (find-employee-record k-name files)
  (cond ((null? files) false)
        ((get-record k-name (car files)) 
          (get-record k-name (car files))) ; data exists, return that. improve by using let
        (else (find-employee-record k-name (cdr files)))))


; d. if the company wants to take a new company, ..
; > need the company's record.
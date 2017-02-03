;; helpers.scm
;; Contains miscellaneous helper functions.
;; Currently only contains exception handling code.

;; ================== Error Handling & Display ============================

;; Datatype for encapsulating exception information for later
;; display. Has one variant: (ex-val who format data)
;;   who = a symbol specifying the name of the function the exception
;;         occured in.
;;   format = an eopl:printf appropriate format string indicating the
;;            type of error.
;;   data = a list of values to put into the format string when displayed.
(define-datatype except except?
  (except-val
   (who symbol?)
   (format string?)
   (data list?)))

;; Displays the exception message.  The input to this function should
;; be data constructed using (except-val who format data). In the case the
;; data is not given via an except-val, the function assumes the exception
;; was thrown by Scheme (not us) and attempts to display it.
(define display-exception
  (lambda (e)
    (cond
     [(except? e)   ;; Raised by us.
      (cases except e
       [except-val (who format data) 
		   (apply eopl:printf (cons format data))
		   (newline)])]
     [else          ;; Raised by Scheme.
      (display "Exception thrown by Scheme:")
      (display e)
      (newline)]
     )))

;; Raises an exception.  Use this to indicate that a problem has
;; occured while interpreting.  For example, 
;; (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))
(define raise-exception 
  (lambda (who format . data)
    (raise (except-val who format data))
    ))

;; Overrides sllgen:error to prevent stopping.
(define sllgen:error raise-exception)

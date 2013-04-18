(defun count-digits-of-factorial (positive-integer-arg )
  (if (or (not (integerp positive-integer-arg))
          (< positive-integer-arg 0))
    (error "argument is not a non-negative integer")
    (let* ((fact-of-arg (factorial positive-integer-arg))
           (string-of-factorial (write-to-string fact-of-arg))
           (count-list (compute-list-of-occurrences string-of-factorial)))
      (format t "~%In factorial of ~d, ~
    the frequency of digits is as shown:" positive-integer-arg)
      (print-data count-list))))

(defun factorial (n)
  (cond ((or (not (integerp n))
             (< n 0))
         (error "argument is not a non-negative integer"))
        (t (fact n))))

(defun fact (arg)
  (if (zerop arg)
      1
      (* arg (fact (1- arg)))))

(defun compute-list-of-occurrences (string-of-digits)
  (loop for i from 0 to 9
        collect
        (count (coerce (write-to-string i) 'character)
               string-of-digits)))

(defun print-data (list-of-counts)
  (format t "~% digit frequency percent-of-total")
  (let ((total (apply #'+ list-of-counts))
        (field-width (length (write-to-string ;caculated failed with
                         (reduce #'max list-of-counts)))))
    (dotimes (i 10)
   (let ((count (nth i list-of-counts)))
        (format t "~% ~d     ~vd       ~5,2f%" i field-width count (* 100 (/ count total)))))))

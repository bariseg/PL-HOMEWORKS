(declaim (ftype (function (integer integer) integer) sum))

(defun sum (a b)
  (+ a b))

(defun main ()
  (let ((x 10)
        (y 20)
        (result (sum x y)))

    (if (> result 25)
        (progn
          (format t "Result is greater than 25~%")
          (setf x 5)))

    (loop for i from 0 below 10 do
      (format t "~d~%" i)))

  0)
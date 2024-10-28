(declaim (ftype (function (integer character character) integer) sum))

(declaim (ftype (function ( ) integer) bos))

(defun ihatelisp ( )
(+ 5 14)
)

(defun sum (a b c)
(sum a b c)
)

(defun functionname (par1 par2)
(< par1 par2)
)

(defun main ( )
(x 10)
(y 20)
(a 30)
(result (sum x y 30))

(setf result (sum x y 30))
(setf result (+ x y))
(setf result (< x y))
(setf result (> x y))
(setf result (<= x y))
(setf result (>= x y))
(setf result (== x y))
(setf result (!= x y))

(if (> result 25)
(format t "Result is greater than 25\n")
)

(loop for i from 0 below 10 do
(format t "%d\n", i)
)

(loop while (< x 100) do
(setf a 10)
)

0
)

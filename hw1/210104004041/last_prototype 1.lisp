(load "D:/Codes/Lisp/homework/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)
 
(defpackage :homework
  (:use :cl :cl-ppcre))
(in-package :homework)


(defun delete-output-file ()
  (let ((file "output.lisp"))
    (when (probe-file file)
      (delete-file file)
      (format t "Deleted ~A~%" file))))

(delete-output-file)
 
(defun line-type (line)
  (cond
    ((cl-ppcre:scan "if\\s*\\(" line) 'if)
    ((cl-ppcre:scan "else" line) 'else)
    ((cl-ppcre:scan "for\\s*\\(" line) 'for)
    ((cl-ppcre:scan "while\\s*\\(" line) 'while)
    ;((cl-ppcre:scan "main\.*" line) 'main)
    ((cl-ppcre:scan "\\w+\\s*\\w+\\s*=\\s*\\w+\\(\.*\\);" line) 'variable-assignment-by-function)
    ((cl-ppcre:scan "return\.*" line) 'return) ; return has priority over arithmetical operation
    ((cl-ppcre:scan "\\w+\\s*(\\&\\&|\\|\\|)\\s*\\w+" line) 'logical-operation)
    ((cl-ppcre:scan "\\w+\\s*(==|!=|<=|>=|<|>)\\s*\\w+" line) 'comparison-operation)
    ((cl-ppcre:scan "\\w+\\s*[-+*/%]\\s*\\w+" line) 'arithmetical-operation)
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*=\\s*\\w+\\s*;" line)  'variable-declaration)
    ((cl-ppcre:scan "\\w+\\s*=\\s*\\w+\\s*" line)  'variable-assignment)
    ((cl-ppcre:scan "printf\\s*\\(\"\.*\",\\s*\.*\\s*\\);" line) 'print-function-variable)
    ((cl-ppcre:scan "printf\\s*\\(\"\.*\"\\s*\\);" line) 'print-function-literal)
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*\\(\.*\\)\\s*;" line) 'function-declaration)
    ((cl-ppcre:scan "\\w{1}\\s*\\(.*\\)\\s*;" line) 'function-call)
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*\\(\.*\\)\\s*{" line) 'function-definition)
    ((cl-ppcre:scan "\\}" line) 'close-brace)
    ((cl-ppcre:scan "\\{" line) 'open-brace)
    ((cl-ppcre:scan "\\w+;" line) 'return-value)
    (t 'unknown))
)
 
(defun convert-function-definition (line)
  (let* ((parts (cl-ppcre:split "^\\w+ " line))
         (return-type (first parts))
         (function (second parts))
         (function-name (first (cl-ppcre:split "\\(" function)))
         (formatted-params (convert-params-with-type-to-lisp-version function)))

      (format nil "(defun ~a ~a" function-name formatted-params))
)

(defun convert-function-declaration (line)
  (let* (
    (parts (cl-ppcre:split " " line :limit 2))
         (return-type (first parts))
         (print return-type)
         (function (second parts))
         (function-name (first (cl-ppcre:split "\\(" function)))
         (return-type-lisp (convert-type-to-lisp-version return-type))
         (params (parse-params-as-list-from-func function))
         (types (parse-types-as-list-from-params-list params))
         (converted-types '()))

         (dolist (type types)
          (let ((converted-type (convert-type-to-lisp-version type)))
            (push converted-type converted-types)
          )
         )
         (format nil "(declaim (ftype (function (~a) ~a) ~a))" (string-join converted-types " ") return-type-lisp function-name )
))


(defun convert-variable-assignment (line)
  (let* ((assignment (cl-ppcre:split "\\s*=\\s*" line))
         (var (string-trim '(#\Newline #\Return #\Space #\Tab) (first assignment)))
         (value (first ( cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab) (second assignment))))))
    (format nil "(setf ~a ~a)" var value))
)
 

(defun convert-function-call (line)
  (let* ((split-line (cl-ppcre:split "\\s*\\(" line))
         (function-name (first split-line))
         (arguments (second split-line)))
    (format nil "(~a ~a)" function-name (string-trim ")" arguments))))
 

(defun convert-print-function-literal (line)
  (let* ((parts (cl-ppcre:split "\"" line))  ; Split by quotes
         (literal (second parts)))            ; Get the first literal part inside the quotes
    (when literal  ; Only process if a match was found
      (format nil "(format t \"~a\")" literal))))



(defun convert-print-function-variable (line)
  (let* ((parts (cl-ppcre:split "\"" line))
         (literal (second parts))
         (after-literal (third parts))
         (variable (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "," after-literal)))))
         (clean-literal (if (cl-ppcre:scan "\\);$" literal)
                            (first (cl-ppcre:split "\\);" literal))
                            literal)))
          (print clean-literal)
          (setf after-literal (replace-percent clean-literal))
          (print after-literal)
    (when (and variable (cl-ppcre:scan "\%[dfi]" clean-literal))
      (let ((formatted-literal (cl-ppcre:split "\%" variable)))
        (format nil "(format t \"~a\" ~a)" after-literal variable)))))
 
(defun replace-percent (input-string)
  (let ((result (make-string (length input-string))))
    (loop for i from 0 below (length input-string)
          do (setf (char result i)
                   (if (char= (char input-string i) #\%)
                       #\~
                       (char input-string i))))                  
    result))

(defun convert-params-without-type-to-lisp-version (params-str)
  (print params-str)
  (let* (
    (params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" params-str)))))
    (arguments (cl-ppcre:split "," params))
    (cleaned-args '()))
   
   (dolist (parameter arguments)
    (let ((trimmed (string-trim " " parameter)))
      (push trimmed cleaned-args)
    )
   )
    (format nil "~a" (string-join cleaned-args " "))
  )
)


(defun convert-params-with-type-to-lisp-version (params-str)
  (print params-str)
  (let* (
    (params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" params-str)))))
    (arguments (cl-ppcre:split "," params))
    (cleaned-args '()))
   
   (dolist (parameter arguments)
    (let ((variable-name (second (cl-ppcre:split " " (string-trim " " parameter)))))
      (push variable-name cleaned-args)
    )
   )
   (print cleaned-args)
    (format nil "(~a)" (string-join cleaned-args " "))
  )
)


(defun parse-params-as-list-from-func (function)
  (let* (
    (params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" function)))))
    (arguments (cl-ppcre:split "," params))
    (cleaned-args '()))
   
   (dolist (parameter arguments)
    (let ((param  (string-trim " " parameter)))
      (push param cleaned-args)
    )
   )
     (nreverse cleaned-args)
  )
)

(defun parse-types-as-list-from-params-list (params)
  (let* (
    (types '()))
   
   (dolist (param params)
    (let ((param  (first (cl-ppcre:split " " (string-trim " " param)))))
      (push param types)
    )
   )
     (nreverse types)
  )
)

(defun convert-type-to-lisp-version (type)
  (let ((lowercase-type (string-downcase type)))  ; Convert to lowercase
    (cond
      ((string= lowercase-type "int") 'integer)
      ((string= lowercase-type "float") 'float)
      ((string= lowercase-type "double") 'double-float)
      ((string= lowercase-type "char") 'character)
      ((string= lowercase-type "void") 'void)
      (t (error "Unknown type: ~a" type)))))  ; Handle unknown types




(defun convert-variable-assignment-by-function (line)
  (let* ((split-line (cl-ppcre:split "=" line))
         (variable-split (cl-ppcre:split " " (first split-line)))
         (variable-name (string-trim " " (second variable-split)))
         (function-call (string-trim " " (second split-line)))
         (function-call-without-sc (string-trim " " (first (cl-ppcre:split ";" function-call))))
         (function-name (first (cl-ppcre:split "\\(" function-call-without-sc)))
         (cleaned-args (convert-params-without-type-to-lisp-version function-call)))
    (format nil "(~a (~a ~a))" variable-name function-name cleaned-args)
  )
)

(defun convert-variable-declaration (line)
  (let* ((split-line (cl-ppcre:split "=" line))
         (variable-split (cl-ppcre:split " " (first split-line)))
         (variable-name (string-trim " " (second variable-split)))
         (assigned-value (first (cl-ppcre:split ";" (string-trim " " (second split-line))))))
    (format nil "(~a ~a)" variable-name assigned-value)
  )
)

(defun string-join (list separator)
  (reduce (lambda (a b)
            (if (string= a "")
                b  ; If 'a' is empty, just return 'b'
                (format nil "~a~a~a" a separator b))) 
          list 
          :initial-value ""))



(defun convert-return (line)
  (let ((value (second (cl-ppcre:split " " line :limit 2))))
    (format nil "~a" (convert-line value))))


(defun convert-return-value (line)
  (let ((value (first (cl-ppcre:split ";" line))))
    (format nil "(~a)" value)))

 
(defun convert-if (line)
  "C dilinde yazılmış bir if ifadesini, uygun şekilde Lisp formatına dönüştürür."
  (let* ((condition (second (cl-ppcre:split "\\s*if\\s*\\(" line)))  
         (cleaned-condition (string-trim ")" condition))
         (parts (cl-ppcre:split "\\s+" cleaned-condition)))  
    (setf cleaned-condition (format nil "~a ~a ~a" (second parts) (first parts) (third parts)))
    (format nil "(if( ~a ~%  (progn ~%    " cleaned-condition)))  
 
 
(defun convert-close-bracket (line)
  "Eğer satırda bir closing brace '}' varsa, satırın sonuna yalnızca bir ')' ekler."
  (if (cl-ppcre:scan "\\s*}\\s*" line)
      (let ((modified-line (cl-ppcre:regex-replace-all "\\s*}\\s*" "" line)))
        (concatenate 'string modified-line ")")
      )
      line))
 
(defun convert-while (line)
  (let ((condition (second (cl-ppcre:split "\\s*while\\s*\\(" line))))
    (format nil "(loop while ~a do)" condition)))
 
 
 

(defun convert-for-loop (line)
  (let* ((parts (cl-ppcre:split "[();]" line))
         (initialization (string-trim " " (nth 1 parts)))  
         (condition (string-trim " " (nth 2 parts)))      
         (increment (string-trim " " (nth 3 parts))))      
    (let* ((init-parts (cl-ppcre:split "\\s+" initialization))
           (var (second init-parts))          
           (init-value (fourth init-parts)))  
      (let ((limit (second (cl-ppcre:split "\\s*<\\s*" condition))))  
        (if (cl-ppcre:scan "\\+\\+" increment)
            (format nil "(loop for ~a from ~a below ~a do" var init-value limit)
            (error "Unsupported increment format: ~a" increment))))))


(defun convert-arithmetical-operation (line)
  (print line)
  (let* ((parts (cl-ppcre:split "\\s*[-+*/%]\\s*" (first (cl-ppcre:split ";" line))))
         (operator (second (cl-ppcre:split " " line)))
         (first-operand (first parts))
         (second-operand (second parts)))
    (format nil "(~a ~a ~a)" operator first-operand second-operand)))

 
 
(defun convert-line (line)
  (let ((type (line-type line)))
    (cond
      ((eq type 'if) (convert-if line))
      ((eq type 'else) (convert-else line))
      ((eq type 'for) (convert-for-loop line))
      ((eq type 'while) (convert-while line))
      ((eq type 'return) (convert-return line))
      ;((eq type 'main) (convert-main line))
      ((eq type 'arithmetical-operation) (convert-arithmetical-operation line))
      ((eq type 'variable-declaration) (convert-variable-declaration line))
      ((eq type 'variable-assignment) (convert-variable-assignment line))
      ((eq type 'function-declaration) (convert-function-declaration line))
      ((eq type 'function-definition) (convert-function-definition line))
      ((eq type 'variable-assignment-by-function) (convert-variable-assignment-by-function line))
      ((eq type 'function-call) (convert-function-call line))
      ((eq type 'print-function-literal) (convert-print-function-literal line))
      ((eq type 'print-function-variable) (convert-print-function-variable line))
      ((eq type 'close-brace) (convert-close-bracket line))
      ((eq type 'open-brace) nil)
      ((eq type 'return-value) (convert-return-value line))
      (t (format nil "Unknown type: ~A, type: ~a" line type))))
)
 
(defun write-to-output (output-line)
  (with-open-file (out "output.lisp" :direction :output :if-does-not-exist :create :if-exists :append)
    (format out "~A~%" output-line)))


(defun process-file-recursively (stream)
  (let ((line (read-line stream nil 'eof)))
    (when (not (eq line 'eof))
      (let ((cleaned-line (clean-line line)))
        (when (not (string= cleaned-line ""))
          (let ((converted-line (convert-line cleaned-line)))
            (when converted-line
              (write-to-output converted-line)
              )))))
    (process-file-recursively stream)))
 
 
(defun process-file (filename)
  (with-open-file (stream filename :direction :input)
    (process-file-recursively stream))) 
 
 
 
(defun clean-line (line)
  (string-trim '(#\Newline #\Return #\Space) line))
 
(process-file "input.c")

; stop the program
(quit)
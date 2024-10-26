(ql:quickload '(:split-sequence :cl-ppcre))


(defun line-type (line)
    (cond
        ((cl-ppcre:scan "[a-zA-Z_]+\\s+[a-zA-Z_]+\\s*\\(.*\\)\\s*;" line) 'function-declaration)
        ((cl-ppcre:scan "[a-zA-Z_]+\\s+[a-zA-Z_]+\\s*\\(.*\\)\\s*\\{" line) 'function-definition)
        ((cl-ppcre:scan "\\s*for\\s*\\(" line) 'for-loop) ; prior to variable-definition
        ((cl-ppcre:scan "[a-zA-Z_]+\\s+[a-zA-Z_]+\\s*=\\s*\\w+\\s*;" line)  'variable-definition) 
        ((cl-ppcre:scan "[a-zA-Z_]+\\s*=\\s*\\w+\\s*;" line) 'assignment)
        ((cl-ppcre:scan "\\s*while\\s*\\(" line) 'while-loop)

        ;;((cl-ppcre:scan "if\\<s*\\(" line) 'if-statement)
        ;;((cl-ppcre:scan "[a-zA-Z_]+\\s*=\\s*[a-zA-Z_]+\\(" line) 'assignment-by-function-return)
        ;;((cl-ppcre:scan "[a-zA-Z_]+\\s*\\(" line) 'function-call)
        ;;((cl-ppcre:scan "[a-zA-Z_]+\\s*[\\+\\-\\*/%]+\\s*[a-zA-Z_]+" line) 'arithmetical-operation)
        ;;((cl-ppcre:scan "[a-zA-Z_]+\\s*[==|!=|<=|>=|<|>]\\s*[a-zA-Z_]+" line) 'logical-operation)
        ((cl-ppcre:scan "\\}" line) 'close-brace)
        (t 'unknown)
    )
)
;(load "newconvert.lisp")
;(main "hoca-input.c" "output.lisp")


(defun convert-closebrace (line)
    (declare (ignore line))
    (format nil ")")
)

;done
(defun convert-while (line)
    (let*
        (
            (condition (second (cl-ppcre:split "\\s*while\\s*\\(" line)))
            (condition (first (cl-ppcre:split "\\)" condition))) ; x < 10
            (var (first (cl-ppcre:split "\\s+" condition))) ; x
            (operator (second (cl-ppcre:split "\\s+" condition))) ; <
            (limit (third (cl-ppcre:split "\\s+" condition))) ; 10
        )
        (format nil "(loop while (~a ~a ~a) do" operator var limit)
    )
)

;done
(defun convert-for (line)
    (let* 
        (
            (parts (cl-ppcre:split "[();]" line))
            (initial (string-trim " " (nth 1 parts)))
            (condition (string-trim " " (nth 2 parts)))
        )   

        (let* 
            (
                (init-parts (cl-ppcre:split "\\s+" initial))
                (var (second init-parts))
                (init-value (fourth init-parts))
            )  

            (let 
                (
                    (limit (second (cl-ppcre:split "\\s*<\\s*" condition)))
                )  
                (format nil "(loop for ~a from ~a below ~a do" var init-value limit)
            )
        )
    )
)

;done
(defun convert-variable-definition (line)
    (let* 
        (
            (variable-definition-content (split-sequence:split-sequence #\= line))
            (variable-part (first variable-definition-content))
            (var-name (string-trim " " (second (cl-ppcre:split " " variable-part))))
            (value 
                (first 
                    (cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab #\=) (second variable-definition-content)))
                )
            )
        )
        (format nil "(~a ~a)" var-name value)
    )
)
;done
(defun convert-assignment (line)
    (let* 
        (
            (assignment-content (split-sequence:split-sequence #\= line))
            (var (string-trim '(#\Newline #\Return #\Space #\Tab) (first assignment-content)))
            (value 
                (first 
                    (cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab #\=) (second assignment-content)))
                )
            )
        )
        (format nil "(setf ~a ~a)" var value)
    )
)
;helper
(defun convert-type-to-lisp-version (type)
    (let ((lowercase-type (string-downcase type)))
        (cond
            ((string= lowercase-type "int") "integer")
            ((string= lowercase-type "float") "float")
            ((string= lowercase-type "double") "double-float")
            ((string= lowercase-type "char") "character")
            ((string= lowercase-type "void") "void")
            (t (error "Unknown type: ~a" type))
        )
    )
)
;helper
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
;helper
(defun parse-params-as-list-from-func (function)
    (let* 
        (
            (params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" function)))))
            (parameters (cl-ppcre:split "," params))
            (cleaned-args '())
        )
        (dolist (parameter parameters)
            (
                let ((param  (string-trim " " parameter)))
                (push param cleaned-args)
            )
        )
        (nreverse cleaned-args)
    )
)
;helper
(defun string-join (list separator)
    (reduce 
        (lambda (a b)
            (if (string= a "")
                b ; return b if a is empty
                (format nil "~a~a~a" a separator b)
            )
        ) 
        list 
        :initial-value ""
    )
)
;done
(defun convert-function-declaration (line)
    (let* 
        (
            (parts (cl-ppcre:split " " line :limit 2)) ;ikiye ayirdik
            
            (return-type (first parts))
            (converted-return-type (convert-type-to-lisp-version return-type))

            (function-part (second parts))
            (function-name (first (cl-ppcre:split "\\(" function-part)))
            (params (parse-params-as-list-from-func function-part))
            (types (parse-types-as-list-from-params-list params))

            (converted-types '()) ;;bos list olusturduk
        )
        (dolist (type types)
            (
                let ((converted-type (convert-type-to-lisp-version type)))
                (push converted-type converted-types)
            )
        )
        (format nil "(declaim (ftype (function (~a) ~a) ~a))" (string-join converted-types " ") converted-return-type function-name )
    )
)

(defun convert-function-definition (line)
    (let* 
        (
            (function-name 
                (first (split-sequence:split-sequence #\Space (subseq line 0 (position #\( line))))
            )
            (function-args 
                (second (split-sequence:split-sequence #\Space (subseq line 0 (position #\{ line))))
            )
        )
        (format nil "(defun ~a ~a" function-name function-args)
    )
)


(defun convert-if (line)
    (let* 
        (
            (ifcondition 
                (second (split-sequence:split-sequence #\Space (subseq line 0 (position #\{ line))))
            )
        )
        (format nil "(if ~a" ifcondition)
    )
)


(defun convert-assignment-by-function-return (line)
    (let* 
        (
            (assignment-content (split-sequence:split-sequence #\= line))
            (var (first assignment-content))
            (function-call (second assignment-content))
        )
        (format nil "(setf ~a ~a)" var function-call)
    )
)

(defun convert-function-call (line)
    (let* 
        (
            (function-call-content 
                (second (split-sequence:split-sequence #\Space (subseq line 0 (position #\) line))))
            )
            (function-name (first (split-sequence:split-sequence #\Space function-call-content)))
            (function-args (second (split-sequence:split-sequence #\Space function-call-content)))
        )
        (format nil "(~a ~a)" function-name function-args)
    )
)


(defun convert-other (line)
  (format nil "unknown line type : ~a" line)
)

(defun conversion-foo (line-type)
    (cond 
        ((eq line-type 'function-declaration) #'convert-function-declaration)
        ((eq line-type 'variable-definition) #'convert-variable-definition)
        ((eq line-type 'assignment) #'convert-assignment)
        ((eq line-type 'close-brace) #'convert-closebrace)
        ((eq line-type 'for-loop) #'convert-for)
        ((eq line-type 'while-loop) #'convert-while)
        ((eq line-type 'function-definition) #'convert-function-definition)
        ;((eq line-type 'if-statement) #'convert-if)
        ;((eq line-type 'assignment-by-function-return) #'convert-assignment-by-function-return)
        ;((eq line-type 'function-call) #'convert-function-call)
        (t #'convert-other)
    )
)

(defun convert (line conversion-fn)
    (funcall conversion-fn line)
)

(defun read_file (file-path)
    (with-open-file (stream file-path)
        (loop for line = (read-line stream nil)
            while line
            collect line
        )
    )
)

(defun write_file (file-path lines)
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
        (dolist 
            (line lines)
            (write-line line stream)
        )
    )
)

(defun clean-line (line)
    (string-trim '(#\Newline #\Return #\Space) line)
)

(defun recursive_convert (lines)
    (if (null lines)
        '()
        (let* 
            (
                (line (first lines))
                (line (clean-line line))
            )
            (if (not (string= line ""))
                (let* 
                    (
                        (line-type-var (line-type line))
                        (conversion-fn (conversion-foo line-type-var))
                        (converted-line (convert line conversion-fn))
                    )
                    (cons converted-line (recursive_convert (rest lines)))
                )
                (recursive_convert (rest lines))
            )
        )
    )
)

(defun main (input-file output-file)
    (let* 
        (
            (lines (read_file input-file))
            (converted-lines (recursive_convert lines))
        )
        (write_file output-file converted-lines)
    )
)
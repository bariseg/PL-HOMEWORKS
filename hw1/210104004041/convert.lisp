(ql:quickload '(:split-sequence :cl-ppcre))


(defun line-type (line)
    (cond
        ((cl-ppcre:scan "^\\s*return\\s+" line) 'return-statement) ; must be prior to function-declaration
        ((cl-ppcre:scan "^\\s*[a-zA-Z_]+\\s+\\w+\\s*\\(.*\\)\\s*;" line) 'function-declaration)
        ((cl-ppcre:scan "^\\s*[a-zA-Z_]+\\s+\\w+\\s*\\(.*\\)\\s*\\{" line) 'function-definition)
        ((cl-ppcre:scan "^\\s*[a-zA-Z_]+\\s*=" line) 'assignment) ; includes assigments by function call, arithmetic operation, logical operation
        ((cl-ppcre:scan "^\\s*\\w+\\s*\\(.*\\)\\s*;" line) 'function-call) ; includes printf, must be prior to if-statement
        ((cl-ppcre:scan "^\\s*if\\s*\\(.*\\)\\s*{" line) 'if-statement)
        ((cl-ppcre:scan "^\\s*[a-zA-Z_]+\\s+\\w+\\s*=" line)  'variable-definition)
        ((cl-ppcre:scan "\\s*for\\s*\\(" line) 'for-loop)
        ((cl-ppcre:scan "\\s*while\\s*\\(" line) 'while-loop)
        ((cl-ppcre:scan "\\}" line) 'close-brace)
        (t 'unknown)
    )
)

;(load "convert.lisp")
;(main "input.c" "output.lisp")

;done
(defun convert-arithmetic-operation (expression)

    (let* 
        (
            (var1 (first (cl-ppcre:split "\\s+" expression))) ; x
            (operator (second (cl-ppcre:split "\\s+" expression))) ; +
            (var2 (third (cl-ppcre:split "\\s+" expression))) ; 10
        )
        
        (format nil "(~a ~a ~a)" operator var1 var2)
    )
)

;done
(defun convert-logical-operation (expression)
    (let* 
        (
            (var1 (first (cl-ppcre:split "\\s+" expression))) ; x
            (operator (second (cl-ppcre:split "\\s+" expression))) ; <
            (var2 (third (cl-ppcre:split "\\s+" expression))) ; 10
            
        )
        (format nil "(~a ~a ~a)" operator var1 var2)
    )
)

;done
(defun convert-closebrace (line)
    (declare (ignore line))
    (format nil ")")
)

;done
(defun convert-while (line)
    (let*
        (
            (condition (second (cl-ppcre:split "\\s*while\\s*\\(\\s*" line)))
            (condition-trimmed (first (cl-ppcre:split "\\s*\\)" condition))) ; x < 10
            (logical-operation (convert-logical-operation condition-trimmed))
        )
        (format nil "(loop while ~a do" logical-operation)
    )
)

;done
(defun convert-for (line)
    (let* 
        (
            (parts (cl-ppcre:split "[();]" line))
            (initial (string-trim " " (nth 1 parts)))
            (condition (string-trim " " (nth 2 parts)))

            (init-parts (cl-ppcre:split "\\s+" initial))
            (var (second init-parts))
            (init-value (fourth init-parts))

            (limit (second (cl-ppcre:split "\\s*<\\s*" condition)))
            
        )
    (format nil "(loop for ~a from ~a below ~a do" var init-value limit)
    )
)

;done
(defun convert-function-call (line)
    (let* 
        (   
            (split-line (cl-ppcre:split "\\s*\\(" line))
            (function-name (first split-line))
            (arguments (second split-line))
            (arguments-trimmed-half (string-trim ";" arguments))
            (arguments-trimmed (string-trim ")" arguments-trimmed-half))
        )

        (if (not (string= function-name "printf"))
            (
                let ((arguments-cleaned (remove #\, arguments-trimmed)))
                (format nil "(~a ~a)" function-name arguments-cleaned)
            )
            (format nil "(format t ~a)" arguments-trimmed)
        )
    )
)

;done
(defun convert-variable-definition (line)
    (let* 
        (
            (variable-definition-content (cl-ppcre:split "\\s*=\\s*" line :limit 2))
            (variable-part (first variable-definition-content))
            (var-name (string-trim " " (second (cl-ppcre:split " " variable-part))))
            
            (trimmed-value (second variable-definition-content))
            (value
                (cond
                    ((cl-ppcre:scan "[a-zA-Z_]+\\s*\\(.*\\)\\s*;" trimmed-value) (convert-function-call trimmed-value))
                    ((cl-ppcre:scan "\\w+\\s*[\\+\\-\\*/%]{1}\\s*\\w+" trimmed-value) (convert-arithmetic-operation (remove #\; trimmed-value)))
                    ((cl-ppcre:scan "\\w+\\s*(<|>|<=|>=|==|!=)\\s*\\w+" trimmed-value) (convert-logical-operation (remove #\; trimmed-value)))
                    (t (first (cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab #\=) (second variable-definition-content)))))
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
            (assignment-content (cl-ppcre:split "\\s*=\\s*" line :limit 2))
            (var (string-trim '(#\Newline #\Return #\Space #\Tab) (first assignment-content)))

            (trimmed-value (second assignment-content)) ;; .... ;
            (value ;;  .... ;
                (cond
                    ((cl-ppcre:scan "[a-zA-Z_]+\\s*\\(.*\\)\\s*;" trimmed-value) (convert-function-call trimmed-value))
                    ((cl-ppcre:scan "\\w+\\s*[\\+\\-\\*/%]{1}\\s*\\w+" trimmed-value) (convert-arithmetic-operation (remove #\; trimmed-value)))
                    ((cl-ppcre:scan "\\w+\\s*(<|>|<=|>=|==|!=)\\s*\\w+" trimmed-value) (convert-logical-operation (remove #\; trimmed-value)))
                    (t (first (cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab #\=) (second assignment-content)))))
                )
            )
        )
        (format nil "(setf ~a ~a)" var value)
    )
)

;helper
(defun convert-types (type)
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
(defun parse-types (params)
    (let* 
        (
            (types '())
        )
        (dolist (param params)
            (
                let ((param-trimmed  (first (cl-ppcre:split " " (string-trim " " param)))))
                (push param-trimmed types)
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

;done
(defun convert-function-declaration (line)
    (let* 
        (
            (parts (cl-ppcre:split " " line :limit 2)) ;ikiye ayirdik
            
            (return-type (first parts))
            (converted-return-type (convert-types return-type))

            (function-part (second parts))
            (function-name (first (cl-ppcre:split "\\(" function-part)))

            (params (parse-params-as-list-from-func function-part))
            (types (parse-types params))

            (converted-types ;; for the immututability of the list we are creating a new list
                (mapcar 
                    (
                        lambda (type) 
                        (convert-types type)
                    )
                    types
                )
            )
        )
        (format nil "(declaim (ftype (function ~a ~a) ~a))" (or converted-types '(" ")) converted-return-type function-name )
    )
)

;done
(defun convert-function-definition (line)
    (let* 
        (
            (parts (cl-ppcre:split " " line :limit 2))

            (function-part (second parts))
            (function-name (first (cl-ppcre:split "\\(" function-part)))
            (params (parse-params-as-list-from-func function-part))

            (param-names ;; for the immututability of the list we are creating a new list
                (mapcar 
                    (
                        lambda (param) 
                        (second (cl-ppcre:split "\\s+" param))
                    )
                    params
                )
            )
        )
        (format nil "(defun ~a ~a" function-name (or param-names '(" "))) ;;parantezler listten geliyor
    )
)


(defun convert-if (line)
    (let* 
        (
            (condition (second (cl-ppcre:split "\\s*if\\s*\\(" line)))
            (condition-trimmed (first (cl-ppcre:split "\\s*\\)" condition))) ; x < 10
            (logical-operation (convert-logical-operation condition-trimmed))
        )
        (format nil "(if ~a" logical-operation)
    )
)

;done
(defun convert-return (line)
    (let* 
        (
            (return-expression (second (cl-ppcre:split "return\\s*" line)))
            (value ;;  .... ;
                (cond
                    ((cl-ppcre:scan "[a-zA-Z_]+\\s*\\(.*\\)\\s*;" return-expression) (convert-function-call return-expression))
                    ((cl-ppcre:scan "\\w+\\s*[\\+\\-\\*/%]{1}\\s*\\w+" return-expression) (convert-arithmetic-operation (remove #\; return-expression)))
                    ((cl-ppcre:scan "\\w+\\s*(<|>|<=|>=|==|!=)\\s*\\w+" return-expression) (convert-logical-operation (remove #\; return-expression)))
                    (t (first (cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab #\=) return-expression))))
                )
            )
        )
        (format nil "~a" value)
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
        ((eq line-type 'function-call) #'convert-function-call)
        ((eq line-type 'arithmetical-operation) #'convert-arithmetic-operation)
        ((eq line-type 'logical-operation) #'convert-logical-operation)
        ((eq line-type 'function-definition) #'convert-function-definition)
        ((eq line-type 'return-statement) #'convert-return)
        ((eq line-type 'if-statement) #'convert-if)
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
            (if (string= line "")
                (cons "" (recursive_convert (rest lines)))
                (let* 
                    (
                        (line-type-var (line-type line))
                        (conversion-fn (conversion-foo line-type-var))
                        (converted-line (convert line conversion-fn))
                    )
                    (cons converted-line (recursive_convert (rest lines)))
                )
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
    "success"
)
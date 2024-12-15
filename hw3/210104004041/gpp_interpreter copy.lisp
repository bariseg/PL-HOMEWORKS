;; the most boring homework ever
;; baris eren gezici

;; tokenizer part is the same as the previous homework
(defparameter *gpp-keywords* '("and" "or" "not" "equal" "less" "nil" "list"
                               "append" "concat" "set" "deffun" "for" "if"
                               "exit" "load" "print" "true" "false"))

(defparameter *gpp-operators* '("+" "-" "*" "/" "(" ")" ","))

(defun classify-token (token)
    (let ((token-str (string token))) ;; Convert token to string for comparison
        (cond
            ;; Check if the token is a keyword
            (   
                (member token-str *gpp-keywords* :test #'string=)
                (intern (format nil "KW_~A" (string-upcase token-str)))
            )
        
            ;; Check if the token is an operator
            ((string= token-str "+") (list 'OP_PLUS 0))
            ((string= token-str "-") 'OP_MINUS)
            ((string= token-str "*") 'OP_MULT)
            ((string= token-str "/") 'OP_DIV)
            ((string= token-str "(") (list 'OP_OP 0))
            ((string= token-str ")") (list 'OP_CP 0))
            ((string= token-str ",") 'OP_COMMA)

            ;; Check for fractions (e.g., 123:45)
            (
                (and 
                    (find #\: token-str)
                    (
                        let* (
                            (colon-pos (position #\: token-str))
                            (num1 (subseq token-str 0 colon-pos))
                            (num2 (subseq token-str (1+ colon-pos)))
                        )
                        (and 
                            (not (string= num1 "")) ;; Ensure num1 is not empty
                            (not (string= num2 "")) ;; Ensure num2 is not empty
                            (every #'digit-char-p num1)
                            (every #'digit-char-p num2)
                        )
                    )
                )
                (list 'VALUEF (/ (parse-integer (subseq token-str 0 (position #\: token-str))) (parse-integer (subseq token-str (1+ (position #\: token-str))))))
            )
            
            ;; Check for integers (e.g., 123)
            ((every #'digit-char-p token-str) 'VALUEI)
            
            ;; Check for valid identifiers (e.g., my_var123)
            (
                (and (alpha-char-p (aref token-str 0))
                    (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) token-str)
                )
                (list 'IDENTIFIER token-str)
            )
            
            ;; Otherwise, it's a syntax error
            (t 'SYNTAX_ERROR)
        )
    )
)

(defun tokenize (line)
    (
        let ((tokens '()) (current-token ""))

        (if (and (>= (length line) 2) (string= (subseq line 0 2) ";;"))
            (push 'COMMENT tokens)

            ;;if not comment, process the line for tokens
            (loop for char across line do
                (cond
                    ;; Ignore whitespace
                    (
                        (member char '(#\Space #\Tab #\Newline #\Return))
                        (
                            when (not (string= current-token ""))
                            (push (classify-token current-token) tokens)
                            (setf current-token "")
                        )
                    )

                    ;; operators
                    (
                        (member (string char) *gpp-operators* :test #'string=)
                        (when (not (string= current-token ""))
                            (push (classify-token current-token) tokens)
                            (setf current-token "")
                        )
                        (push (classify-token (string char)) tokens)
                    )

                    ;; if the string is now a token yet, we collect them
                    ;; Collect characters for tokens
                    (t (setf current-token (concatenate 'string current-token (string char))))
                )
            )
        )
        ;; Push the last token if any
        ;; for example, if the line ends with a identifier "baris" we need to push it
        ;;because (t (setf current-token (concatenate 'string current-token (string char)))) part didnt push it
        (when (not (string= current-token ""))
            (push (classify-token current-token) tokens)
        )
        (reverse tokens)
    )
)

;; parser part : the new implemented part for this homework

;; expression -> OP_OP OP_PLUS expression expression OP_CP
(defun reduce-expression-op-plus (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'OP_PLUS)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (+ expr1 expr2))
            )
            (print (format nil "expression(plus) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression (+ expr1 expr2)) stack)
        )
    )
    stack)
)

;; expression -> OP_OP OP_MINUS expression expression OP_CP
(defun reduce-expression-op-minus (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'OP_MINUS)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(minus)")
        (setf stack (nthcdr 5 stack))  
        (push 'expression stack))      
      stack))

;; expression -> OP_OP OP_MULT expression expression OP_CP
(defun reduce-expression-op-mult (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'OP_MULT)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(mult)")
        (setf stack (nthcdr 5 stack))
        (push 'expression stack))
      stack))

;; expression -> OP_OP OP_DIV expression expression OP_CP
(defun reduce-expression-op-div (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'OP_DIV)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(div)")
        (setf stack (nthcdr 5 stack))  
        (push 'expression stack))      
      stack))

;;expression -> OP_OP KW_SET IDENTIFIER expression OP_CP
(defun reduce-expression-op-set (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_SET)
           (eql (nth 2 stack) 'IDENTIFIER)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(set)")
        (setf stack (nthcdr 5 stack))  
        (push 'expression stack))
      stack))

;; expression -> IDENTIFIER
(defun reduce-expression-identifier (stack)
  (if (and (>= (length stack) 1)
           (eql (nth 0 stack) 'IDENTIFIER))
      (progn
        (print "expression(identifier)")
        (setf stack (nthcdr 1 stack)) 
        (push 'expression stack)) 
      stack))

;; expression -> VALUEF
(defun reduce-expression-valuef (stack)
    (if (and 
            (>= (length stack) 1)
            (eql (first (nth 0 stack)) 'VALUEF)
        )
    (progn
        (let
            (
                (valuef (second (nth 0 stack)))
            )
            (print (format nil "expression(valuef) : ~a" valuef))
            (setf stack (nthcdr 1 stack))
            (push (list 'expression valuef) stack)
        )

    ) 
    stack)
)

;;expression -> OP_OP KW_DEFFUN IDENTIFIER OP_OP identifier_list OP_CP expression_list OP_CP
(defun reduce-expression-deffun (stack)
  (if (and (>= (length stack) 8)
           (eql (nth 7 stack) 'OP_OP)
           (eql (nth 6 stack) 'KW_DEFFUN)
           (eql (nth 5 stack) 'IDENTIFIER)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'identifier_list)
           (eql (nth 2 stack) 'OP_CP)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(deffun)")
        (setf stack (nthcdr 8 stack))  
        (push 'expression stack))   
      stack))

;; expression -> OP_OP IDENTIFIER expression_list OP_CP
(defun reduce-expression-identifier-list (stack)
  (if (and (>= (length stack) 4)
           (eql (nth 3 stack) 'OP_OP)
           (eql (nth 2 stack) 'IDENTIFIER)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(identifier-list)")
        (setf stack (nthcdr 4 stack)) 
        (push 'expression stack))
      stack))

;;expression -> OP_OP KW_IF expression_boolean expression_list OP_CP
(defun reduce-expression-if (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_IF)
           (eql (nth 2 stack) 'expression_boolean)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(if)")
        (setf stack (nthcdr 5 stack)) 
        (push 'expression stack))  
      stack))

;;expression -> OP_OP KW_IF expression_boolean expression_list expression_list OP_CP
(defun reduce-expression-if-else (stack)
  (if (and (>= (length stack) 6)
           (eql (nth 5 stack) 'OP_OP)
           (eql (nth 4 stack) 'KW_IF)
           (eql (nth 3 stack) 'expression_boolean)
           (eql (nth 2 stack) 'expression_list)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(if-else)")
        (setf stack (nthcdr 6 stack)) 
        (push 'expression stack))
      stack))

;;expression -> OP_OP KW_WHILE expression_boolean expression_list OP_CP
(defun reduce-expression-while (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_WHILE)
           (eql (nth 2 stack) 'expression_boolean)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(while)")
        (setf stack (nthcdr 5 stack))
        (push 'expression stack))
      stack))

;; expression -> OP_OP KW_FOR OP_OP IDENTIFIER expression expression OP_CP expression_list OP_CP
(defun reduce-expression-for (stack)
  (if (and (>= (length stack) 9)
           (eql (nth 8 stack) 'OP_OP)
           (eql (nth 7 stack) 'KW_FOR)
           (eql (nth 6 stack) 'OP_OP)
           (eql (nth 5 stack) 'IDENTIFIER)
           (eql (nth 4 stack) 'expression)
           (eql (nth 3 stack) 'expression)
           (eql (nth 2 stack) 'OP_CP)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(for)")
        (setf stack (nthcdr 9 stack))
        (push 'expression stack))
      stack))

;;expression -> OP_OP KW_DEFVAR IDENTIFIER expression OP_CP
(defun reduce-expression-defvar (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_DEFVAR)
           (eql (nth 2 stack) 'IDENTIFIER)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(defvar)")
        (setf stack (nthcdr 5 stack))
        (push 'expression stack))
      stack))

;; expression_list -> expression_list expression
(defun reduce-expression-list (stack)
  (if (and (>= (length stack) 2)
           (eql (nth 1 stack) 'expression_list)
           (eql (nth 0 stack) 'expression))
      (progn
        (print "expression_list")
        (setf stack (nthcdr 2 stack))
        (push 'expression_list stack))
      stack))

;; expression_list -> expression
(defun reduce-expression-list-single (stack)
    (if (and    
            (= (length stack) 1) 
            (eql (first (nth 0 stack)) 'expression)
        )
        (progn
            (let 
                ((value (second (nth 0 stack))))
                (print (format nil "expression_list(single) : ~a" value))
                (setf stack (nthcdr 1 stack))
                (push (list 'expression_list value) stack)
            )
        )
    stack
    )
)

;; expression_boolean -> OP_OP KW_AND expression expression OP_CP
(defun reduce-expression-boolean-and (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_AND)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression_boolean(and)")
        (setf stack (nthcdr 5 stack))
        (push 'expression_boolean stack))
      stack))

;; expression_boolean -> OP_OP KW_OR expression expression OP_CP
(defun reduce-expression-boolean-or (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_OR)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression_boolean(or)")
        (setf stack (nthcdr 5 stack))
        (push 'expression_boolean stack))
      stack))

;;expression_boolean -> OP_OP KW_NOT expression OP_CP
(defun reduce-expression-boolean-not (stack)
  (if (and (>= (length stack) 4)
           (eql (nth 3 stack) 'OP_OP)
           (eql (nth 2 stack) 'KW_NOT)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression_boolean(not)")
        (setf stack (nthcdr 4 stack)) 
        (push 'expression_boolean stack))
      stack))

;; expression_boolean -> OP_OP KW_EQUAL expression expression OP_CP
(defun reduce-expression-boolean-equal (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_EQUAL)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression_boolean(equal)")
        (setf stack (nthcdr 5 stack)) 
        (push 'expression_boolean stack))
      stack))

;;expression_boolean -> OP_OP KW_LESS expression expression OP_CP
(defun reduce-expression-boolean-less (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'KW_LESS)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression_boolean(less)")
        (setf stack (nthcdr 5 stack))
        (push 'expression_boolean stack))
      stack)
)


; (defun reduce-identifier-list (stack)
;   (if (and (>= (length stack) 2)
;            (eql (nth (- (length stack) 2) stack) 'identifier_list)
;            (eql (nth (- (length stack) 1) stack) 'IDENTIFIER))
;       (progn
;         (print "identifier_list")
;         (setf stack (nthcdr 2 stack)) 
;         (push 'identifier_list stack))
;       stack))


; (defun reduce-identifier-list-single (stack)
;   (if (and (>= (length stack) 1)
;            (eql (nth (- (length stack) 1) stack) 'IDENTIFIER))
;       (progn
;         (print "identifier_list(single)")
;         (setf stack (nthcdr 1 stack))  
;         (push 'identifier_list stack)) 
;       stack))


(defun reduce-input (stack)
    (if (and 
            (= (length stack) 1) 
            (eql (first (nth 0 stack)) 'expression_list) 
        )
        
        (progn
            (let 
                ((value (second (nth 0 stack))))
                (print (format nil "input : ~a" value))
                (setf stack (nthcdr 1 stack))
                (push (list 'input value) stack)
            )
        )
    stack
    )
)

(defun reduce-start (stack)
    (if (and (= (length stack) 1) (eql (first (nth 0 stack)) 'input))
        (progn
            (let 
                ((value (second (nth 0 stack))))
                (print (format nil "start : ~a" value))
                (setf stack (nthcdr 1 stack))
                (push (list 'start value) stack)
            )
        )
    stack
    )
)

(defun shift (stack token)
    (push token stack)
    stack
)

(defun reducer (stack)
  (let ((new-stack stack))
    
    ;;(print (format nil "Current stack: ~a" stack))

    (setq new-stack (reduce-expression-op-plus new-stack))
    (setq new-stack (reduce-expression-op-minus new-stack))
    (setq new-stack (reduce-expression-op-mult new-stack))
    (setq new-stack (reduce-expression-op-div new-stack))
    (setq new-stack (reduce-expression-op-set new-stack))
    (setq new-stack (reduce-expression-identifier new-stack))
    (setq new-stack (reduce-expression-valuef new-stack))
    (setq new-stack (reduce-expression-deffun new-stack))
    (setq new-stack (reduce-expression-identifier-list new-stack))
    (setq new-stack (reduce-expression-if new-stack))
    (setq new-stack (reduce-expression-if-else new-stack))
    (setq new-stack (reduce-expression-while new-stack))
    (setq new-stack (reduce-expression-for new-stack))
    (setq new-stack (reduce-expression-defvar new-stack))
    (setq new-stack (reduce-expression-list new-stack))
    (setq new-stack (reduce-expression-list-single new-stack))
    (setq new-stack (reduce-expression-boolean-and new-stack))
    (setq new-stack (reduce-expression-boolean-or new-stack))
    (setq new-stack (reduce-expression-boolean-not new-stack))
    (setq new-stack (reduce-expression-boolean-equal new-stack))
    (setq new-stack (reduce-expression-boolean-less new-stack))
    ;;(setq new-stack (reduce-identifier-list new-stack))
    ;;(setq new-stack (reduce-identifier-list-single new-stack))
    (setq new-stack (reduce-input new-stack))
    (setq new-stack (reduce-start new-stack))

    ;;(print (format nil "New stack: ~a" new-stack))
    new-stack
    )
)

(defun parser (input)
    (let 
        ((stack '()))
        
        (dolist (token input)
            (setq stack (shift stack token))
            
            (let 
                ((prev-stack nil))
                (loop do
                    (setq prev-stack stack)
                    (setq stack (reducer stack))
                    until (equal stack prev-stack)
                )
            )
        )
    stack)
)

(defun gppinterpreter ()
    (format t "Enter your input: ")
    (force-output)
    (let 
        (
            (line (read-line))
            (result-stack nil)
        )
    (setq result-stack (parser (tokenize line)))
    (print result-stack)
    
    (if (equal (first (nth 0 result-stack)) 'start)
        (print "Parser success")
        (print "Parser failed")
    )
    )
)

(defun gppinterpreter (&optional (filename nil))
    (let 
        (
            (lines 
                (if filename
                    (with-open-file (stream filename)
                        (loop for line = (read-line stream nil)
                            while line
                            collect line
                        )
                    )
                    (list 
                        (progn
                            (format t "Enter your input: ")
                            (force-output)
                            (read-line)
                        )
                    )
                )
            )
            (result-stack nil)
        )
        (dolist (line lines)
            
            (setq result-stack (parser (tokenize line)))
            (print result-stack)
            
            (if (equal (first (nth 0 result-stack)) 'start)
                (print "Parser success")
                (print "Parser failed")
            )
        )
    t ;; avoid returning nil
    )
)

;; (load "gpp_interpreter.lisp")
;; (gppinterpreter "input.txt")
;; (gppinterpreter)

; (let 
;     (
;         (input "(+ 1:2 2:3)")
;         (result-stack nil)
;     )
;     (setq result-stack (parser (tokenize input)))
;     (print result-stack)
    
;     (if (equal result-stack '(start))
;         (print "Parser success")
;         (print "Parser failed")
;     )
; )
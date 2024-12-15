;; the most boring homework ever
;; baris eren gezici

;; tokenizer part is the modified version of the previous homework
(defparameter *gpp-keywords* '("and" "or" "not" "equal" "less" "nil" "list" "while"
                               "append" "concat" "set" "deffun" "defvar" "for" "if"
                               "exit" "load" "print" "true" "false"))

(defparameter *gpp-operators* '("+" "-" "*" "/" "(" ")" ","))

(defun classify-token (token)
    (let ((token-str (string token))) ;; Convert token to string for comparison
        (cond
            ;; keyword
            (   
                (member token-str *gpp-keywords* :test #'string=)
                (list (intern (format nil "KW_~A" (string-upcase token-str))) 0)
            )
        
            ;; operator
            ((string= token-str "+") (list 'OP_PLUS 0))
            ((string= token-str "-") (list 'OP_MINUS 0))
            ((string= token-str "*") (list 'OP_MULT 0))
            ((string= token-str "/") (list 'OP_DIV 0))
            ((string= token-str "(") (list 'OP_OP 0))
            ((string= token-str ")") (list 'OP_CP 0))
            ((string= token-str ",") (list 'OP_COMMA 0))

            ;;fractions 123:45
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
                            (not (string= num1 "")) 
                            (not (string= num2 "")) 
                            (every #'digit-char-p num1)
                            (every #'digit-char-p num2)
                        )
                    )
                )
                (list 'VALUEF (/ (parse-integer (subseq token-str 0 (position #\: token-str))) (parse-integer (subseq token-str (1+ (position #\: token-str))))))
            )
            
            ;; identifiers
            (
                (and (alpha-char-p (aref token-str 0))
                    (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) token-str)
                )
                (list 'IDENTIFIER token-str)
            )
            
            ;; Otherwise, it's a syntax error
            (t (list 'SYNTAX_ERROR 0))
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
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'OP_MINUS)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (- expr1 expr2))
            )
            (print (format nil "expression(minus) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression result) stack)
        )
    )
    stack)
)

;; expression -> OP_OP OP_MULT expression expression OP_CP
(defun reduce-expression-op-mult (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'OP_MULT)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (* expr1 expr2))
            )
            (print (format nil "expression(mult) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression result) stack)
        )
    )
    stack)
)

;; expression -> OP_OP OP_DIV expression expression OP_CP
(defun reduce-expression-op-div (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'OP_DIV)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (/ expr1 expr2))
            )
            (print (format nil "expression(div) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression result) stack)
        )
    )
    stack)
)
                            ;; identifier
;;expression -> OP_OP KW_SET expression expression OP_CP
(defun reduce-expression-op-set (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (nth 4 stack) 'OP_OP)
            (eql (nth 3 stack) 'KW_SET)
            (eql (nth 2 stack) 'expression)
            (eql (nth 1 stack) 'expression)
            (eql (nth 0 stack) 'OP_CP))
        (progn
            (let* 
                (
                    (value (second (nth 1 stack)))
                )
                (print (format nil "expression(set) : ~a" value))
                (setf stack (nthcdr 5 stack))
                (push (list 'expression value) stack)
            )
        )
    stack
    )
)


;; expression -> IDENTIFIER
(defun reduce-expression-identifier (stack)
    (if (and 
            (>= (length stack) 1)
            (eql (first (nth 0 stack)) 'IDENTIFIER)
        )
    (progn
        (let
            (
                (identifier (second (nth 0 stack)))
            )
            (print (format nil "expression(identifier) : ~a" identifier))
            (setf stack (nthcdr 1 stack))
            (push (list 'expression 0) stack)
        )
    )
    stack)
)

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
;; (gppinterpreter "input.txt") identifier   ;; identifier_list  expression_list
;;expression -> OP_OP KW_DEFFUN expression OP_OP expression OP_CP expression OP_CP
(defun reduce-expression-deffun (stack)
    (if (and 
            (>= (length stack) 8)
            (eql (first (nth 7 stack)) 'OP_OP)
            (eql (first (nth 6 stack)) 'KW_DEFFUN)
            (eql (first (nth 5 stack)) 'expression)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'expression)
            (eql (first (nth 2 stack)) 'OP_CP)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 1 stack)))
            )
            (print (format nil "expression(deffun) : ~a " value))
            (setf stack (nthcdr 8 stack))
            (push (list 'expression value) stack)
        )
    )
    stack)
)

;; expression -> OP_OP identifier_list IDENTIFIER OP_CP
(defun reduce-expression-identifier-list (stack)
    (if (and 
            (>= (length stack) 4)
            (eql (first (nth 3 stack)) 'OP_OP)
            (eql (first (nth 2 stack)) 'identifier_list)
            (eql (first (nth 1 stack)) 'IDENTIFIER)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 1 stack)))
            )
            (print (format nil "expression(identifier-list) : ~a" value))
            (setf stack (nthcdr 4 stack))
            (push (list 'expression value ) stack)
        )
    )
    stack)
)
;; (load "gpp_interpreter.lisp")
;; (gppinterpreter "input.txt")
                                            ;; expression_list
;;expression -> OP_OP KW_IF expression_boolean expression OP_CP
(defun reduce-expression-if (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_IF)
            (eql (first (nth 2 stack)) 'expression_boolean)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 2 stack)))
            )
            (print (format nil "expression(if) : ~a" value))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression value) stack)
        )
    )
    stack)
)
                                            ;; expression_list expression_list
;;expression -> OP_OP KW_IF expression_boolean expression expression OP_CP
(defun reduce-expression-if-else (stack)
    (if (and 
            (>= (length stack) 6)
            (eql (first (nth 5 stack)) 'OP_OP)
            (eql (first (nth 4 stack)) 'KW_IF)
            (eql (first (nth 3 stack)) 'expression_boolean)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 3 stack)))
            )
            (print (format nil "expression(if-else) : ~a" value))
            (setf stack (nthcdr 6 stack))
            (push (list 'expression value) stack)
        )
    )
    stack)
)
;; (load "gpp_interpreter.lisp")
;;(gppinterpreter "input.txt")
                                            ;; expression_list
;;expression -> OP_OP KW_WHILE expression_boolean expression OP_CP
(defun reduce-expression-while (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_WHILE)
            (eql (first (nth 2 stack)) 'expression_boolean)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 2 stack)))
            )
            (print (format nil "expression(while) : ~a" value))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression value) stack)
        )
    )
    stack)
)
                                ;; yacc fileda IDENTIFIER                   expression_list
;; expression -> OP_OP KW_FOR OP_OP expression expression expression OP_CP expression OP_CP
(defun reduce-expression-for (stack)
    (if (and 
            (>= (length stack) 9)
            (eql (first (nth 8 stack)) 'OP_OP)
            (eql (first (nth 7 stack)) 'KW_FOR)
            (eql (first (nth 6 stack)) 'OP_OP)
            (eql (first (nth 5 stack)) 'expression)
            (eql (first (nth 4 stack)) 'expression)
            (eql (first (nth 3 stack)) 'expression)
            (eql (first (nth 2 stack)) 'OP_CP)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 1 stack)))
            )
            (print (format nil "expression(for) : ~a " value))
            (setf stack (nthcdr 9 stack))
            (push (list 'expression value) stack)
        )
    )
    stack)
)
                            ;; yacc fileda IDENTIFIER
;;expression -> OP_OP KW_DEFVAR expression expression OP_CP
(defun reduce-expression-defvar (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_DEFVAR)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let
            (
                (value (second (nth 1 stack)))
            )
            (print (format nil "expression(defvar) : ~a" value))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression value) stack)
        )
    )
    stack)
)

;; expression_list -> expression_list expression
(defun reduce-expression-list (stack)
    (if (and 
            (>= (length stack) 2)
            (eql (first (nth 1 stack)) 'expression_list)
            (eql (first (nth 0 stack)) 'expression)
        )
    (progn
        (let
            (
                (value (second (nth 0 stack)))
            )
            (print (format nil "expression_list : ~a"  value))
            (setf stack (nthcdr 2 stack))
            (push (list 'expression_list value) stack)
        )
    )
    stack)
)

;; expression_list -> expression
(defun reduce-expression-list-single (stack)
    (if (and    
            (= (length stack) 1) 
            (eql (first (nth 0 stack)) 'expression)
        )
        (progn
            (let 
                (
                    (value (second (nth 0 stack)))
                )
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
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_AND)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (and expr1 expr2))
            )
            (print (format nil "expression_boolean(and) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression_boolean result) stack)
        )
    )
    stack)
)

;; expression_boolean -> OP_OP KW_OR expression expression OP_CP
(defun reduce-expression-boolean-or (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_OR)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (or expr1 expr2))
            )
            (print (format nil "expression_boolean(or) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression_boolean result) stack)
        )
    )
    stack)
)

;;expression_boolean -> OP_OP KW_NOT expression OP_CP
(defun reduce-expression-boolean-not (stack)
    (if (and 
            (>= (length stack) 4)
            (eql (first (nth 3 stack)) 'OP_OP)
            (eql (first (nth 2 stack)) 'KW_NOT)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr (second (nth 1 stack)))
                (result (not expr))
            )
            (print (format nil "expression_boolean(not) : ~a" result))
            (setf stack (nthcdr 4 stack))
            (push (list 'expression_boolean result) stack)
        )
    )
    stack)
)

;; expression_boolean -> OP_OP KW_EQUAL expression expression OP_CP
(defun reduce-expression-boolean-equal (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_EQUAL)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (equal expr1 expr2))
            )
            (print (format nil "expression_boolean(equal) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression_boolean result) stack)
        )
    )
    stack)
)

;;expression_boolean -> OP_OP KW_LESS expression expression OP_CP
(defun reduce-expression-boolean-less (stack)
    (if (and 
            (>= (length stack) 5)
            (eql (first (nth 4 stack)) 'OP_OP)
            (eql (first (nth 3 stack)) 'KW_LESS)
            (eql (first (nth 2 stack)) 'expression)
            (eql (first (nth 1 stack)) 'expression)
            (eql (first (nth 0 stack)) 'OP_CP)
        )
    (progn
        (let*
            (
                (expr1 (second (nth 2 stack)))
                (expr2 (second (nth 1 stack)))
                (result (< expr1 expr2))
            )
            (print (format nil "expression_boolean(less) : ~a" result))
            (setf stack (nthcdr 5 stack))
            (push (list 'expression_boolean result) stack)
        )
    )
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
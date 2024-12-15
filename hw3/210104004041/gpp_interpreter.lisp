
;;tokenize operations are the same with the homework 2

(defparameter *gpp-keywords* '("and" "or" "not" "equal" "less" "nil" "list"
                               "append" "concat" "set" "deffun" "for" "if"
                               "exit" "load" "print" "true" "false")
)

(defparameter *gpp-operators* '("+" "-" "*" "/" "(" ")" ","))


;; classify a token as keyword, operator, literal, identifier, or syntax error.
(defun classify-token (token)
    (let ((token-str (string token))) ;; Convert token to string for comparison
        (cond
            ;; Check if the token is a keyword
            (   
                (member token-str *gpp-keywords* :test #'string=)
                (intern (format nil "KW_~A" (string-upcase token-str)))
            )
        
            ;; Check if the token is an operator
            ((string= token-str "+") 'OP_PLUS)
            ((string= token-str "-") 'OP_MINUS)
            ((string= token-str "*") 'OP_MULT)
            ((string= token-str "/") 'OP_DIV)
            ((string= token-str "(") 'OP_OP)
            ((string= token-str ")") 'OP_CP)
            ((string= token-str ",") 'OP_COMMA)

            ;; Check for fractions (e.g., 123:45)
            (
                (and (find #\: token-str)
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
                'VALUEF
            )
            
            ;; Check for valid identifiers (e.g., my_var123)
            (
                (and (alpha-char-p (aref token-str 0))
                    (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) token-str)
                )
                'IDENTIFIER
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

;; (load "gpp_interpreter.lisp")


(defun make-node (name value &rest children)
  "Create a parse tree node with a name, value, and children."
  (list :name name :value value :children children))

(defun print-tree (node depth)
  "Print the parse tree."
  (when node
    (dotimes (i depth) (format t "| "))
    (format t "~A : ~A~%" (getf node :name) (getf node :value))
    (dolist (child (getf node :children))
      (print-tree child (1+ depth)))))

(defun parse-shift-reduce (tokens)
  (let ((stack '()))
    (loop
      (cond
        ;; Reduce: expression -> OP_OP OP_PLUS expression expression OP_CP
        ((and (>= (length stack) 5)
              (equal (first (nthcdr 4 stack)) 'OP_OP)
              (equal (first (nthcdr 3 stack)) 'OP_PLUS)
              (equal (first stack) 'OP_CP))
         (let ((left (second (nthcdr 2 stack)))
               (right (second stack)))
           (setf stack (append (butlast stack 5)
                               (list (make-node "expression(plus)" (+ (getf left :value) (getf right :value)) left right))))))

        ;; Reduce: expression -> OP_OP OP_MINUS expression expression OP_CP
        ((and (>= (length stack) 5)
              (equal (first (nthcdr 4 stack)) 'OP_OP)
              (equal (first (nthcdr 3 stack)) 'OP_MINUS)
              (equal (first stack) 'OP_CP))
         (let ((left (second (nthcdr 2 stack)))
               (right (second stack)))
           (setf stack (append (butlast stack 5)
                               (list (make-node "expression(minus)" (- (getf left :value) (getf right :value)) left right))))))

        ;; Reduce: expression -> OP_OP OP_MULT expression expression OP_CP
        ((and (>= (length stack) 5)
              (equal (first (nthcdr 4 stack)) 'OP_OP)
              (equal (first (nthcdr 3 stack)) 'OP_MULT)
              (equal (first stack) 'OP_CP))
         (let ((left (second (nthcdr 2 stack)))
               (right (second stack)))
           (setf stack (append (butlast stack 5)
                               (list (make-node "expression(mult)" (* (getf left :value) (getf right :value)) left right))))))

        ;; Reduce: expression -> OP_OP OP_DIV expression expression OP_CP
        ((and (>= (length stack) 5)
              (equal (first (nthcdr 4 stack)) 'OP_OP)
              (equal (first (nthcdr 3 stack)) 'OP_DIV)
              (equal (first stack) 'OP_CP))
         (let ((left (second (nthcdr 2 stack)))
               (right (second stack)))
           (setf stack (append (butlast stack 5)
                               (list (make-node "expression(div)" (/ (getf left :value) (getf right :value)) left right))))))

        ;; Reduce: expression_list -> expression
        ((and (>= (length stack) 1)
              (equal (first stack) 'expression))
         (let ((expr (first stack)))
           (setf stack (append (butlast stack 1)
                               (list (make-node "expression_list" (getf expr :value) expr))))))

        ;; Reduce: expression_list -> expression_list expression
        ((and (>= (length stack) 2)
              (equal (first stack) 'expression)
              (equal (second stack) 'expression_list))
         (let ((expr (first stack))
               (expr-list (second stack)))
           (setf stack (append (butlast stack 2)
                               (list (make-node "expression_list" (getf expr :value) expr-list expr))))))

        ;; Reduce: input -> expression_list
        ((and (>= (length stack) 1)
              (equal (first stack) 'expression_list))
         (let ((expr-list (first stack)))
           (setf stack (append (butlast stack 1)
                               (list (make-node "input" (getf expr-list :value) expr-list))))))

        ;; Reduce: start -> input
        ((and (>= (length stack) 1)
              (equal (first stack) 'input))
         (let ((input (first stack)))
           (setf stack (append (butlast stack 1)
                               (list (make-node "start" (getf input :value) input))))))

        ;; Shift: Push the next token onto the stack
        (tokens
         (push (first tokens) stack)
         (setf tokens (rest tokens)))

        ;; Finish: Stop when only one node is left
        ((= (length stack) 1)
         (return (first stack)))

        (t (error "Parse error: ~A" stack))))))

;; Example Usage
(let ((parse-tree (parse-shift-reduce (tokenize "( + 5 3 )"))))
  (print-tree parse-tree 0))
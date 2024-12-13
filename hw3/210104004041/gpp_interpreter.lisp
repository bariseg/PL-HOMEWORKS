
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


;;(load "gpp_lexer.lisp")
;;(gppinterpreter)
;;(gppinterpreter "test.txt")

;; Parse Tree Functions
(defun make-node (name &rest children)
  "Create a parse tree node with a name and children."
  (list name children))

;; Shift-Reduce Parser
(defun parse-shift-reduce (tokens)
  
  (let 
    ((stack '())) ;; Parse stack
    (loop
      (cond
        ;; Reduce: Match expressions like ( + exp exp )
        (
          (and 
              (>= (length stack) 5)                   ;;   5      4     3   2    1
              (equal (first (nthcdr 4 stack)) 'OP_OP) ;; OP_OP OP_PLUS exp exp OP_CP
              (member (first (nthcdr 3 stack)) '(OP_PLUS OP_MINUS OP_MULT OP_DIV))
              (equal (first stack) 'OP_CP)
          )
          
          (let* 
            (
              (operator (first (nthcdr 3 stack)))
              (left (first (nthcdr 2 stack)))
              (right (first (nthcdr 1 stack)))
            )
            (setf stack (append (butlast stack 5) (list (make-node operator left right))))
          )
        )

        ;; Reduce: Match set statements
        (
          (and                    
            (>= (length stack) 5)                   ;; 5  4  3 2 1
            (equal (first (nthcdr 4 stack)) 'OP_OP) ;; ( set x 4 )
            (equal (first (nthcdr 3 stack)) 'KW_SET)
            (equal (first stack) 'OP_CP)
          )
          
          (let* 
            (
              (identifier (first (nthcdr 3 stack)))
              (expression (second stack))
            )
            
            (setf stack (append (butlast stack 5) (list (make-node 'set identifier expression))))
          )
        )

        ;; Reduce: Match if statements
        (
          (and 
            (>= (length stack) 7)
            (equal (first (nthcdr 6 stack)) 'OP_OP)
            (equal (third (nthcdr 6 stack)) 'KW_IF)
            (equal (first stack) 'OP_CP)
          )
          (let* 
            (
              (condition (second (nthcdr 4 stack)))
              (true-branch (second (nthcdr 2 stack)))
              (false-branch (if (= (length stack) 7) nil (second stack)))
            )
            (setf stack (append (butlast stack 7)  (list (make-node 'if condition true-branch false-branch))))
          )
        )

        ;; Shift: Push the next token onto the stack
        (
          tokens
          (push (first tokens) stack)
          (setf tokens (rest tokens))
        )

        ;; Finish: Stop when only one node is left
        (
          (= (length stack) 1)
          (return (first stack))
        )

        (t (error "Parse error: ~A" stack))
      )
    )
  )
)

;; Entry Point
(defun interpret-line (input)
  "Interpret a single line of G++ code."
  (let ((tokens (tokenize input)))
    (parse-shift-reduce tokens)))

;; Example Usage
(format t "~%Parse Tree: ~A~%" (interpret-line "( + 5 ( * 2 3 ) )"))
(format t "~%Parse Tree: ~A~%" (interpret-line "( if ( less 5 10 ) 1 0 )"))
(format t "~%Parse Tree: ~A~%" (interpret-line "( set x ( + 1 2 ) )"))
(format t "~%Parse Tree: ~A~%" (interpret-line "( + 10 20 )"))
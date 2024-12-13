(defparameter *gpp-keywords* '("and" "or" "not" "equal" "less" "nil" "list"
                               "append" "concat" "set" "deffun" "for" "if"
                               "exit" "load" "print" "true" "false")
)

(defparameter *gpp-operators* '("+" "-" "*" "/" "(" ")" ","))

;;(load "gpp_lexer.lisp")
;;(gppinterpreter)
;;(gppinterpreter "test.txt")


;;Classify a token as keyword, operator, literal, identifier, or syntax error.
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
            
            ;; Check for integers (e.g., 123)
            ((every #'digit-char-p token-str) 'VALUEI)
            
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



;;Start the G++ interpreter. Reads from a file if provided, or interactively.
(defun gppinterpreter (&optional filename)
    (if filename
        ;; File input mode
        (with-open-file (stream filename)
            (loop for line = (read-line stream nil nil)
                while line
                do (print (tokenize line))
            )
        )
        ;; Interactive mode
        (loop
            ;; Print prompt before reading user input
            (format t "> ")
            (force-output) ;; print hemen gozuksun diye
            (let ((line (read-line nil nil)))
                (
                    when line
                    (print (tokenize line))
                    (terpri)
                )
            )
        )
    )
)

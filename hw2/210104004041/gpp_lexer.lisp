(defparameter *gpp-keywords* '("and" "or" "not" "equal" "less" "nil" "list"
                               "append" "concat" "set" "deffun" "for" "if"
                               "exit" "load" "print" "true" "false")
)

(defparameter *gpp-operators* '("+" "-" "*" "/" "(" ")" ","))

;;(load "gpp_lexer.lisp")
;;(gppinterpreter)
;;(gppinterpreter "test.gpp")


;;Classify a token as keyword, operator, literal, identifier, or syntax error.
(defun classify-token (token)
    (let ((token-str (string token)))
        (cond
            ((member token-str  *gpp-keywords*) (intern (format nil "KW_~A" (string-upcase token-str ))))
            ((member token-str  *gpp-operators*) (intern (format nil "OP_~A" (string-upcase token-str ))))
            ((find #\: token-str ) 'VALUEF) ;; Fractions contain ':'
            ((every #'digit-char-p token-str ) 'VALUEI) ;; Integers
            (
                (and    (alpha-char-p (aref token-str 0))
                        (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) token-str )
                )
                'IDENTIFIER
            ) ;; Valid identifiers
            
            (t 'SYNTAX_ERROR)
        ) ;; Syntax error
    )
)

(defun tokenize (line)

    (
        ;;initialized tokens and current-token
        let ((tokens '()) (current-token ""))

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

                ;; Comments (start with ;;)
                (
                    (and (char= char #\;) (not (string= current-token "")))
                    (push 'COMMENT tokens)
                    (return)
                )

                ;; Operators
                (
                    (member (string char) *gpp-operators*)
                    (when (not (string= current-token ""))
                        (push (classify-token current-token) tokens)
                        (setf current-token "")
                    )
                    (push (classify-token (string char)) tokens)
                )

                ;; Collect characters for tokens
                (t (setf current-token (concatenate 'string current-token (string char))))
            )
            
        )

        ;; Push the last token if any
        (when (not (string= current-token ""))
            (push (classify-token current-token) tokens)
        )

        (reverse tokens)
    )
)

;;(load "gpp_lexer.lisp")
;;(gppinterpreter)
;;(gppinterpreter "test.gpp")



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




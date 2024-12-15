;;;; G++ Syntax Analyzer in Lisp

;; Tokenizer Functions
(defparameter *gpp-keywords* '("and" "or" "not" "equal" "less" "nil" "list"
                               "append" "concat" "set" "deffun" "for" "if"
                               "exit" "load" "print" "true" "false"))

(defparameter *gpp-operators* '("+" "-" "*" "/" "(" ")" ","))

(defun classify-token (token)
  "Classify a token as a keyword, operator, identifier, or literal."
  (cond
   ;; Keywords
   ((member token *gpp-keywords* :test #'string=)
    (intern (format nil "KW_~A" (string-upcase token))))
   ;; Operators
   ((member token *gpp-operators* :test #'string=)
    (case token
      ("+" 'OP_PLUS) ("-" 'OP_MINUS) ("*" 'OP_MULT) ("/" 'OP_DIV)
      ("(" 'OP_OP) (")" 'OP_CP) ("," 'OP_COMMA)))
   ;; Fractions (e.g., 123:45)
   ((string-match-p "^[0-9]+:[0-9]+$" token)
    'VALUEF)
   ;; Integers
   ((string-match-p "^[0-9]+$" token)
    'VALUEI)
   ;; Identifiers
   ((string-match-p "^[a-zA-Z_][a-zA-Z0-9_]*$" token)
    'IDENTIFIER)
   ;; Syntax Error
   (t 'SYNTAX_ERROR)))

(defun tokenize (line)
  "Tokenize an input line."
  (let ((tokens '()) (current-token ""))
    (if (and (>= (length line) 2) (string= (subseq line 0 2) ";;"))
        (list 'COMMENT)
      (progn
        (loop for char across line do
          (cond
           ;; Whitespace
           ((member char '(#\Space #\Tab #\Newline #\Return))
            (when (not (string= current-token ""))
              (push (classify-token current-token) tokens)
              (setf current-token "")))
           ;; Operators
           ((member (string char) *gpp-operators* :test #'string=)
            (when (not (string= current-token ""))
              (push (classify-token current-token) tokens)
              (setf current-token ""))
            (push (classify-token (string char)) tokens))
           ;; Collect characters for tokens
           (t (setf current-token (concatenate 'string current-token (string char))))))
        ;; Push the last token if any
        (when (not (string= current-token ""))
          (push (classify-token current-token) tokens))
        (reverse tokens)))))

;; Parse Tree Functions
(defun make-node (name value &rest children)
  "Create a parse tree node with a name, value, and children."
  (list name value children))

(defun print-parse-tree (tree depth)
  "Recursively print the parse tree with indentation."
  (when tree
    (format t "~A~A : ~A~%" (make-string (* depth 2) #\Space) (first tree) (second tree))
    (dolist (child (third tree))
      (print-parse-tree child (1+ depth))))) 

;; Shift-Reduce Parser
(defun parse-shift-reduce (tokens)
  "Parse the tokens using shift-reduce algorithm."
  (let ((stack '())) ;; Parse stack
    (loop
      (cond
       ;; Reduce: Match expressions like ( + exp exp )
       ((and (>= (length stack) 5)
             (equal (first (nthcdr 3 stack)) 'OP_OP)
             (member (third (nthcdr 3 stack)) '(OP_PLUS OP_MINUS OP_MULT OP_DIV))
             (equal (first stack) 'OP_CP))
        (let* ((operator (third (nthcdr 3 stack)))
               (left (second (nthcdr 3 stack)))
               (right (second (nthcdr 1 stack)))
               (value (case operator
                        (OP_PLUS (+ (second left) (second right)))
                        (OP_MINUS (- (second left) (second right)))
                        (OP_MULT (* (second left) (second right)))
                        (OP_DIV (/ (second left) (second right))))))
          (setf stack (append (butlast stack 5)
                              (list (make-node operator value left right))))))

       ;; Reduce: Match set statements
       ((and (>= (length stack) 5)
             (equal (first (nthcdr 3 stack)) 'OP_OP)
             (equal (third (nthcdr 3 stack)) 'KW_SET)
             (equal (first stack) 'OP_CP))
        (let* ((identifier (second (nthcdr 3 stack)))
               (expression (second (nthcdr 1 stack))))
          (setf stack (append (butlast stack 5)
                              (list (make-node 'set expression identifier))))))

       ;; Reduce: Match if statements
       ((and (>= (length stack) 7)
             (equal (first (nthcdr 6 stack)) 'OP_OP)
             (equal (third (nthcdr 6 stack)) 'KW_IF)
             (equal (first stack) 'OP_CP))
        (let* ((condition (second (nthcdr 4 stack)))
               (true-branch (second (nthcdr 2 stack)))
               (false-branch (if (= (length stack) 7) nil (second stack))))
          (setf stack (append (butlast stack 7)
                              (list (make-node 'if condition true-branch false-branch))))))

       ;; Shift: Push the next token onto the stack
       (tokens
        (push (first tokens) stack)
        (setf tokens (rest tokens)))

       ;; Finish: Stop when only one node is left
       ((= (length stack) 1) (return (first stack)))

       (t (error "Parse error: ~A" stack))))))

;; Entry Point
(defun interpret-line (input)
  "Interpret a single line of G++ code."
  (let ((tokens (tokenize input)))
    (let ((tree (parse-shift-reduce tokens)))
      (print-parse-tree tree 0))))

;; Example Usage
(interpret-line "( + 5 ( * 2 3 ) )")
(interpret-line "( if ( less 5 10 ) 1 0 )")
(interpret-line "( set x ( + 1 2 ) )")
(interpret-line "( + 10 20 )")

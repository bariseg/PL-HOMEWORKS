

;; expression -> OP_OP OP_PLUS expression expression OP_CP
(defun reduce-expression-op-plus (stack)
  (if (and (>= (length stack) 5)
           (eql (nth 4 stack) 'OP_OP)
           (eql (nth 3 stack) 'OP_PLUS)
           (eql (nth 2 stack) 'expression)
           (eql (nth 1 stack) 'expression)
           (eql (nth 0 stack) 'OP_CP))
      (progn
        (print "expression(plus)")
        (setf stack (nthcdr 5 stack))  
        (push 'expression stack))      
      stack))

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
  (if (and (>= (length stack) 1)
           (eql (nth 0 stack) 'VALUEF))
      (progn
        (print "expression(valuef)")
        (setf stack (nthcdr 1 stack)) 
        (push 'expression stack)) 
      stack))

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
        (push 'expression stack)
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
  (if (and (= (length stack) 1)
           (eql (nth 0 stack) 'expression))
      (progn
        (print "expression_list(single)")
        (setf stack (nthcdr 1 stack))
        (push 'expression_list stack))
      stack))

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
      stack))


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
  (if (and (eql (car stack) 'expression_list) (= (length stack) 1))
      (progn
        (print "input")
        (list 'input))
      stack))


(defun reduce-start (stack)
  (if (eql (car stack) 'input)
      (progn
        (print "start")
        (list 'start))
      stack))

;;; Shift operation: Add a token to the stack
(defun shift (stack token)
  (push token stack)
  stack)

;;; Reduce operation: Apply reduction rules based on the stack
(defun reducer (stack)
  "Reduce operation based on the stack."
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

;;; Define the reduction functions for each rule


;;; Main Parser
(defun parser (input)
  "Parser function to process the tokens and perform shift/reduce operations."
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


;; OP_OP KW_SET IDENTIFIER OP_OP OP_PLUS VALUEF VALUEF OP_CP OP_CP
;; OP_OP OP_PLUS expression OP_OP OP_PLUS VALUEF IDENTIFIER OP_CP OP_CP

;; (load "last.lisp")

(let 
    (
        (input '(OP_OP KW_WHILE expression_boolean expression_list OP_CP))
        (result-stack nil)
    )
    (setq result-stack (parser input))
    (print result-stack)
    
    (if (equal result-stack '(start))
        (print "Parser success")
        (print "Parser failed")
    )
)


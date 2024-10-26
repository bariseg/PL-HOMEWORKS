(defun split-by-char (char string)
  (let ((pos (position char string)))
    (if pos
        (list (subseq string 0 pos) (subseq string (1+ pos)))
        (list string))))

(defun line-type (line)
  (cond ((search "if (" line) 'if-statement)
        ((search "for (" line) 'for-loop)
        ((search "while (" line) 'while-loop)
        ((search "=" line) 'assignment)
        ((search "(" line) 'function-call)
        (t 'unknown)))

(defun conversion-foo (line-type)
  (cond ((eq line-type 'if-statement) #'convert-if)
        ((eq line-type 'for-loop) #'convert-for)
        ((eq line-type 'assignment) #'convert-assignment)
        ((eq line-type 'function-call) #'convert-function-call)
        (t #'convert-unknown)))

(defun convert (line)
  (let* ((line-type (line-type line))
         (conversion-fn (conversion-foo line-type)))
    (funcall conversion-fn line)))

(defun convert-if (line)
  (let ((condition (extract-condition line)))
    (format nil "(if ~a)" condition)))

(defun extract-condition (line)
  (subseq line (position #\( line) (position #\{ line))))

(defun convert-for (line)
  (let ((init (extract-init line))
        (test (extract-test line))
        (update (extract-update line)))
    (format nil "(loop for ~a ~a until ~a do ~a)" init update test update)))

(defun extract-init (line)
  (first (split-by-char #\; (subseq line (position #\( line))))))

(defun extract-test (line)
  (second (split-by-char #\; (subseq line (position #\( line))))))

(defun extract-update (line)
  (third (split-by-char #\; (subseq line (position #\( line)))))

(defun convert-assignment (line)
  (let ((var (extract-variable line))
        (value (extract-value line)))
    (format nil "(setf ~a ~a)" var value)))

(defun extract-variable (line)
  (first (split-by-char #\= line)))

(defun extract-value (line)
  (second (split-by-char #\= line)))

(defun convert-function-call (line)
  (let ((function-name (subseq line 0 (position #\( line))))
    (format nil "(~a)" function-name)))

(defun convert-unknown (line)
  (format nil ";; Unknown or unsupported line: ~a" line))

(defun process-file (lines)
  (if (null lines)
      '()
      (let ((converted-line (convert (first lines))))
        (cons converted-line (process-file (rest lines))))))

(defun read-file (file-path)
  (with-open-file (in file-path)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun write-file (file-path content)
  (with-open-file (out file-path :direction :output :if-exists :supersede)
    (dolist (line content)
      (write-line line out))))

(defun convert-c-to-lisp (input-file output-file)
  (let ((lines (read-file input-file)))
    (let ((converted-lines (process-file lines)))
      (write-file output-file converted-lines))))

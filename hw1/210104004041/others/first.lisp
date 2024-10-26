;; line-type: C dilindeki satırın türünü belirler
(defun line-type (line)
  (cond ((string-match "if\\s*(" line) 'if-statement)
        ((string-match "for\\s*(" line) 'for-loop)
        ((string-match "while\\s*(" line) 'while-loop)
        ((string-match "[a-zA-Z_]+\\s*=\\s*" line) 'assignment)
        ((string-match "[a-zA-Z_]+\\s*\\(" line) 'function-call)
        (t 'unknown)))

;; conversion-foo: Satır türüne göre dönüştürme fonksiyonunu seçer
(defun conversion-foo (line-type)
  (cond ((eq line-type 'if-statement) #'convert-if)
        ((eq line-type 'for-loop) #'convert-for)
        ((eq line-type 'assignment) #'convert-assignment)
        ((eq line-type 'function-call) #'convert-function-call)
        (t #'convert-unknown)))

;; convert: Satırı uygun dönüştürme fonksiyonuyla Lisp'e çevirir
(defun convert (line)
  (let* ((line-type (line-type line))
         (conversion-fn (conversion-foo line-type)))
    (funcall conversion-fn line)))

;; convert-if: C dilindeki if yapısını Lisp'e çevirir
(defun convert-if (line)
  (let ((condition (extract-condition line)))
    (format nil "(if ~a" condition)))

;; extract-condition: if satırındaki koşulu çıkarır
(defun extract-condition (line)
  (second (split-sequence:split-sequence #\()  ; "(" işaretinden sonra koşulu alır
           (subseq line 0 (position #\{ line)))))  ; "{" öncesini alır

;; convert-for: C dilindeki for döngüsünü Lisp'e çevirir
(defun convert-for (line)
  (let ((init (extract-init line))
        (test (extract-test line))
        (update (extract-update line)))
    (format nil "(loop for ~a ~a until ~a do ~a)" init update test update)))

(defun extract-init (line)
  (second (split-sequence:split-sequence #\; line)))

(defun extract-test (line)
  (third (split-sequence:split-sequence #\; line)))

(defun extract-update (line)
  (fourth (split-sequence:split-sequence #\; line)))

;; convert-assignment: C dilindeki değişken atamasını Lisp'e çevirir
(defun convert-assignment (line)
  (let ((var (extract-variable line))
        (value (extract-value line)))
    (format nil "(setf ~a ~a)" var value)))

(defun extract-variable (line)
  (first (split-sequence:split-sequence #\= line)))

(defun extract-value (line)
  (second (split-sequence:split-sequence #\= line)))

;; convert-function-call: C'deki fonksiyon çağrısını Lisp'e çevirir
(defun convert-function-call (line)
  (let ((function-name (subseq line 0 (position #\( line))))
    (format nil "(~a)" function-name)))

;; convert-unknown: Tanımlanamayan satırları olduğu gibi döndürür
(defun convert-unknown (line)
  (format nil ";; Unknown or unsupported line: ~a" line))

;; process-file: Dosyadaki satırları dönüştürüp sonuçları bir liste olarak döndürür
(defun process-file (lines)
  (if (null lines)
      '()  ;; Taban durumu, satır kalmadığında boş liste döndür
      (let ((converted-line (convert (first lines))))
        (cons converted-line (process-file (rest lines))))))

;; read-file: C dosyasını satır satır okur
(defun read-file (file-path)
  (with-open-file (in file-path)
    (loop for line = (read-line in nil)
          while line
          collect line)))

;; write-file: Dönüştürülen Lisp kodunu bir dosyaya yazar
(defun write-file (file-path content)
  (with-open-file (out file-path :direction :output :if-exists :supersede)
    (dolist (line content)
      (write-line line out))))

;; Ana fonksiyon: C dosyasını alır, Lisp'e çevirir ve çıktıyı yazar
(defun convert-c-to-lisp (input-file output-file)
  (let ((lines (read-file input-file))
        (converted-lines (process-file lines)))
    (write-file output-file converted-lines)))

;; Örnek kullanım:
;; (convert-c-to-lisp "input.c" "output.lisp")

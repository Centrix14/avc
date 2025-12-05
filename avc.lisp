;;;; utils

(defun nil-as (alias value)
  "interprets value as alias if value == nil"
  (if value value alias))

(defun as-logical (value)
  "interprets nil and other values as nil and T"
  (if value t nil))

;; todo: rewrite into macro
(defun one-of-strings (value list)
  (declare (type string value))
  (as-logical (member value list :test #'string=)))

(defmacro define-string-of-p (name predicates chars)
  `(defun ,name (value)
     (declare (type string value))

     (loop for char across value do
	   (unless (or ,@(map 'list
			      (lambda (f)
				(list f 'char))
			      predicates)
		       ,@(map 'list
			      (lambda (c)
				(list 'char= c 'char))
			      chars))
	     (return-from ,name nil)))
     t))

(defun read-non-empty-line (&optional (prompt ""))
  (do ((input "" (read-line)))
      ((not (string= input "")) input)
    (princ prompt)))

(defun file-exists-p (path)
  (handler-case (probe-file path)
    (condition () nil)))

;;;; todo:
;;;; 1) выполнить все имеющиеся todo
;;;; 2) добавить интерфейс с запуском (s), окончанием (e) и превалидацией (pv) - она должна обнаруживать неправильные записи и только

(defparameter *municipality-types* '("г" "пгт" "с" "д"))
(defparameter *path-types* '("ул" "б-р" "ш"))
(defparameter *building-types* '("д" "корп" "стр"))

(defparameter *source-file* nil)
(defparameter *destination-file* nil)

(defparameter *pattern* '(post-index-p municipality-type-p toponymp path-type-p toponymp building-type-p building-number-p anythingp))

(defparameter *current-line-verbatim* "")
(defparameter *current-line-form* '(""))

(defparameter *field-separators* ",.")

;;;; validation functions

(defun validate (pattern addr)
  "validates addr by given pattern"
  (declare (type list pattern addr))

  (let ((i 0)
        log arity matcher)
    
    (dolist (rule pattern)

      (setf arity 1)

      (cond
        
        ((symbolp rule) (setf matcher rule))

        ((listp rule)
         (unless (or (eql 'n (second rule)) ;; unless arity == n | integer
                     (integerp (second rule)))
           (error "Arity must be integer or 'n"))
         
         (setf matcher (first rule))
         (setf arity (nil-as 0 (second rule))))

        (t (error "Invalid pattern rule ~a" rule)))

      (if (eql arity 'n)
	  
          (do () ;; arity == n -> (incf i) until matcher returns T
              ((or (= i (length addr))
                   (not (funcall matcher (nth i addr)))))
            (incf i))
	  
          (do ((n 0 (1+ n))) ;; arity is an integer -> (incf i) arity times
	      ((or (= n arity)
		   (= i (length addr))))
	    
            (unless (funcall matcher (nth i addr))
              (push (list rule i (nth i addr)) log))
            (incf i))))

    (values (null log) log)))

(defun captured-validate (pattern addr)
  (multiple-value-bind (result log) (validate pattern addr)
    (unless result
      (princ log)
      (terpri))
    result))

;;;; address parts predicates

(defun anythingp (value)
  "envelopes any readable Lisp value"
  (declare (ignore value))
  t)

(defun post-index-p (value)
  "returns T for post-indexes (6 digits)"
  (declare (type string value))
  
  (loop for char across value do ;; todo: extract pattern into macro
        (unless (digit-char-p char)
          (return-from post-index-p nil)))
  
  (= 6 (length value)))

(defun municipality-type-p (value)
  "returns T for values from *municipality-types*"
  (one-of-strings value *municipality-types*))

(defun toponymp (value)
  "returns T for values = [a-zA-Z0-9\- ]"
  (declare (type string value))

  (loop for char across value do
    
        (unless (or (alphanumericp char)
                    (char= #\Space char)
                    (char= #\- char))
          (return-from toponymp nil)))
  t)

(defun path-type-p (value)
  "returns T for values from *path-types*"
  (one-of-strings value *path-types*))

(defun building-type-p (value)
  "returns T for values from *building-types*"
  (one-of-strings value *building-types*))

(define-string-of-p building-number-p (alphanumericp) (#\/ #\- #\Space))

;;;; operational functions

(defun start-file ()
  
  (let (source-file-name destination-file-name)

    (do ()
	((file-exists-p source-file-name))

      (princ "--- Файл-источник должен существовать, назначение может быть создано автоматически")
      (terpri)
      
      (setf source-file-name (read-non-empty-line "Введите имя файла-источника: "))
      (setf destination-file-name (read-non-empty-line "Введите имя файла-назначения: ")))

    (setf *source-file* (open source-file-name
			      :direction :input))
    (setf *destination-file* (open destination-file-name
				   :direction :output
				   :if-exists :supersede
				   :if-does-not-exist :create)))
  t)

(defun end-file ()
  (when *source-file*
    (close *source-file*)
    (setf *source-file* nil))
  
  (when *destination-file*
    (close *destination-file*)
    (setf *destination-file* nil)))

(defun next-line ()
  (if *source-file*
      (let ((line (read-line *source-file* nil 'eof)))
        (if (stringp line)
            (setf *current-line-verbatim* line)
            (format t "! Достигнут конец файла")))

      (format t "! Файл-источник не назначен~%")))

(defun separatorp (char)
  (as-logical (position char *field-separators* :test #'char=)))

(defun %split-string% (string &key (separatorp #'separatorp))
  (loop :for beg = (position-if-not separatorp string)
	:then (position-if-not separatorp string :start (1+ end))
	:for end = (and beg (position-if separatorp string :start beg))
	:when beg :collect (subseq string beg end)
	:while end))

(defun divide-line ()
  (setf *current-line-form*
	(map 'list
	     (lambda (str)
	       (string-trim " " str))
	     (%split-string% *current-line-verbatim*))))

(defun validate-line ()
  (validate *pattern* *current-line-form*))

(defun instant-correct-line ()

  (prin1 *current-line-form*)
  (terpri)

  (setf *current-line-form*
	(do ((form *current-line-form*
		   (read-from-string
		    (read-non-empty-line "Введите откорректированную форму: "))))
	    ((captured-validate *pattern* form) form)
	  ())))

(defun output-line ()

  (dolist (elm *current-line-form*)
    (format *destination-file* "~a, " elm))
  
  (not (format *destination-file* "~%")))

(defun output-valid-lines ()

  (do ((line "" (read-line *source-file* nil "eof"))
       (line-form '("") (map 'list
			     (lambda (str)
			       (string-trim " " str))
			     (%split-string% line)))
       )
      ((string= line "eof"))

    (when (validate *pattern* line-form)
      (setf *current-line-form* line-form)
      (output-line)
      )
    ))
;; (format t "~a ~a~%" (validate *pattern* line-form) line)
;;; commands

(define-symbol-macro q (quit))

(define-symbol-macro s (start-file))
(define-symbol-macro e (end-file))

(define-symbol-macro nl (next-line))
(define-symbol-macro n
    (progn
      (when *current-line-verbatim*
	(divide-line)
	(if (validate *pattern* *current-line-form*)
	    (progn
	      (output-line)
	      (next-line))
	    (format t "Адрес содержит ошибки~%")))))

(define-symbol-macro ol (output-line))

(define-symbol-macro dl (divide-line))
(define-symbol-macro vl (validate-line))
(define-symbol-macro icl (instant-correct-line))

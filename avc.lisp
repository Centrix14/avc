;;;; todo:
;;;; - добавить общую функцию очистки с flatten
;;;; - параметризовать все комманды

;;;; programm parameters

(defparameter *pattern* '(post-index-p municipality-type-p toponymp path-type-p toponymp building-type-p building-number-p anythingp))

(defparameter *municipality-types* '("г" "пгт" "с" "д"))
(defparameter *path-types* '("ул" "б-р" "ш"))
(defparameter *building-types* '("д" "корп" "стр"))

(defparameter *source-file* nil)
(defparameter *destination-file* nil)

(defparameter *current-line-verbatim* "")
(defparameter *current-line-form* '(""))

(defparameter *field-separators* ",.")

(defparameter *frequent-adhesives* '("город" "проезд" "бульвар" "проспект" "улица" "площадь" "шоссе" "переулок" "набережная" "дом" "строение" "корпус"))

;;;; utils

(defun nil-as (alias value)
  (if value value alias))

(defun as-logical (value)
  (if value t nil))

(defmacro define-one-of-p (name list test)
  `(defun ,name (value)
     (declare (type string value))
     (as-logical (member value ,list :test ,test))))

(defmacro define-string-of-p (name predicates chars &body post-conditions)
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

     ,@post-conditions))

(defmacro defcommand (command-name function-name lambda-list &body body)
  `(progn
    (defun ,function-name ,lambda-list ,@body)
    (define-symbol-macro ,command-name (,function-name))))

(defun read-non-empty-line (&optional (prompt ""))
  (do ((input "" (read-line)))
      ((not (string= input "")) input)
    (princ prompt)))

(defun file-exists-p (path)
  (handler-case (probe-file path)
    (condition () nil)))

(defun clean-and-trim-list (list)
  (do ((i 0 (1+ i))
       elm cleared-elm result)
      ((= i (length list)) result)

    (setf elm (nth i list))
    (setf cleared-elm
          (if (listp elm)
              (clean-and-trim-list elm)
              (list (string-trim " " elm))))

    (unless (equalp cleared-elm '(""))
      (setf result (append result cleared-elm)))))

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

(defun anythingp (value) (declare (ignore value)) t)

(define-string-of-p post-index-p (digit-char-p) nil
  (= 6 (length value)))

(define-one-of-p municipality-type-p *municipality-types* #'string=)

(define-string-of-p toponymp (alphanumericp) (#\Space #\-) t)

(define-one-of-p path-type-p *path-types* #'string=)

(define-one-of-p building-type-p *building-types* #'string=)

(define-string-of-p building-number-p (alphanumericp) (#\/ #\- #\Space) t)

;;;; commands

(defcommand s start-file ()
  
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

(defcommand e end-file ()
  (when *source-file*
    (close *source-file*)
    (setf *source-file* nil))
  
  (when *destination-file*
    (close *destination-file*)
    (setf *destination-file* nil)))

(defcommand nl next-line ()
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

(defun %split-string-by-string% (main-string separator-string)
  (let ((separator-l (length separator-string))
        (i 0) result)

    (do ((p (search separator-string main-string :start2 0 :test #'char=)))
        ((not p))

      (setf result (append result
                           (list (subseq main-string i p)
                                 separator-string)))

      (incf i (+ separator-l (- p i)))
      (setf p (search separator-string main-string :start2 i :test #'char=)))

    (append result (list (subseq main-string i)))))

(defcommand dl divide-line ()
  (setf *current-line-form*
        (%split-string% *current-line-verbatim*)))

(defcommand cl cleanup-line ()
  (setf *current-line-form*
        (clean-and-trim-list *current-line-form*)))

(defcommand vl validate-line ()
  (validate *pattern* *current-line-form*))

(defcommand icl instant-correct-line ()

  (prin1 *current-line-form*)
  (terpri)

  (setf *current-line-form*
	    (do ((form *current-line-form*
		           (read-from-string
		            (read-non-empty-line "Введите откорректированную форму: "))))
	        ((captured-validate *pattern* form) form)
	      ())))

(defcommand ol output-line ()

  (dolist (elm *current-line-form*)
    (format *destination-file* "~a, " elm))
  
  (not (format *destination-file* "~%")))

(defun %output-validated-like% (&optional (modifier #'values))
  (do ((line "" (next-line)))
      ((null line))

    (divide-line)
    (cleanup-line)
    
    (when (funcall modifier (validate-line))
      (output-line))))

(defcommand ov output-valid-lines () (%output-validated-like%))
(defcommand oi output-invalid-lines () (%output-validated-like% #'not))

(defcommand rdp redivide-part (&optional n separator)
  (unless n
    (setf n (parse-integer
             (read-non-empty-line "№ части для редактирования: "))))
  (unless separator
    (setf separator (read-non-empty-line "Строка-разделитель: ")))
  
  (setf (nth n *current-line-form*)
        (%split-string-by-string% (nth n *current-line-form*) separator)))

(defun %search-adhesive% (str)
  (dotimes (i (length *frequent-adhesives*) nil)

    (when (search (nth i *frequent-adhesives*) str :test #'string=)
      (return-from %search-adhesive% (nth i *frequent-adhesives*)))))

(defcommand rfa redivide-frequent-adhesives ()
  (let (elm adhesive)
    
    (dotimes (i (length *current-line-form*)
                *current-line-form*)

      (setf elm (nth i *current-line-form*))
      
      (setf adhesive (%search-adhesive% elm))
      (when adhesive
        (redivide-part i adhesive)))))

;;;; no-functional commands

(define-symbol-macro q (quit))

(define-symbol-macro n
    (progn
      (when *current-line-verbatim*
	(divide-line)
	(if (validate *pattern* *current-line-form*)
	    (progn
	      (output-line)
	      (next-line))
	    (format t "Адрес содержит ошибки~%")))))

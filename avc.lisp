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
			      chars)))
	   (return-from ,name nil))
     t))

;;;; todo:
;;;; 1) выполнить все имеющиеся todo
;;;; 2) добавить интерфейс с запуском (s), окончанием (e) и превалидацией (pv) - она должна обнаруживать неправильные записи и только

(defparameter +municipality-types+ '("г" "пгт" "с" "д"))
(defparameter +path-types+ '("ул" "б-р" "ш"))
(defparameter +building-types+ '("д" "корп" "стр"))

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
  "returns T for values from +municipality-types+"
  (one-of-strings value +municipality-types+))

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
  "returns T for values from +path-types+"
  (one-of-strings value +path-types+))

(defun building-type-p (value)
  "returns T for values from +building-types+"
  (one-of-strings value +building-types+))

(define-string-of-p building-number-p (alphanumericp) (#\/ #\- #\Space))

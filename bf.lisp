(defun new-memory ()
  (make-array 256 :initial-element 0))

(defun get-memory (memory position)
  (aref memory position))

(defun set-memory (memory position value)
  (let ((memory (make-array 256 :initial-contents memory)))
    (setf (aref memory position) value)
    memory))

(defun bf (source)
  (let ((memory (new-memory))
	(position 0))
    (loop for c across source do (let ((result (process c memory position)))
				   (setf memory (first result))
				   (setf position (second result))))))

(defun process (command memory position)
  ;; Side effects first:
  (cond 
   ((char= command #\.) (print (aref memory position))))

  ;; Now - memory and position:
  (let ((new-position (cond
		       ((char= command #\<) (- position 1))
		       ((char= command #\>) (+ position 1))
		       (t position)))
	(new-memory (let* ((value (get-memory memory position))
			   (new-value (cond
				       ((char= command #\+) (+ value 1))
				       ((char= command #\-) (- value 1))
				       (t value))))
		      (set-memory memory position new-value))))

    ;; Return changed values:
    (list new-memory new-position)))

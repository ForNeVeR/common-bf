(defun new-memory ()
  (make-array 256 :initial-element 0))

(defun bf (source)
  (let ((memory (new-memory))
	(position 0))
    (loop for c across source do (let ((result (process c memory position)))
				   (setf memory (first result))
				   (setf position (second result))))))

(defun process (command memory position)
  (cond
   ((char= command #\<) (decf position))
   ((char= command #\>) (incf position))
   ((char= command #\+) (incf (aref memory position)))
   ((char= command #\-) (decf (aref memory position)))
   ((char= command #\.) (print (aref memory position))))
  (list memory position))
    

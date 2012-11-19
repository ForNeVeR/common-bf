;;; Memory.
(defparameter *memory-size* 256)

(defun new-memory ()
  (make-array *memory-size* :initial-element 0))

(defun memory-get (memory position)
  (aref memory position))

(defun memory-set (memory position value)
  (let ((memory (make-array *memory-size* :initial-contents memory)))
    (setf (aref memory position) value)
    memory))

;;; Stack.
(defun new-stack ()
  '())

(defun stack-push (stack value)
  (cons value stack))

(defun stack-peek (stack)
  (car stack))

(defun stack-pop (stack)
  (cdr stack))

;;; State.
(defstruct bf-state memory pointer position stack)

(defun new-state ()
  (make-bf-state :memory (new-memory)
		 :pointer 0
		 :position 0
		 :stack (new-stack)))

;;; Source.
(defun source-get (source index)
  (char source index))

(defun bf (source)
  (let ((state (new-state)))
    (loop do (let ((command (source-get source (bf-state-position state))))
	       (setf state (process command state))
	       (if (>= (bf-state-position state) (length source))
		   (return state))))))

(defun process (command state)
  (let ((state (copy-bf-state state)))
    (cond
     ((char= command #\<)
      (decf (bf-state-pointer state)))
     ((char= command #\>)
      (incf (bf-state-pointer state)))
     ((char= command #\-)
      (setf (bf-state-memory state)
	    (memory-set (bf-state-memory state)
			(bf-state-pointer state)
			(- (memory-get (bf-state-memory state)
				       (bf-state-pointer state))
			   1))))
     ((char= command #\+)
      (setf (bf-state-memory state)
	    (memory-set (bf-state-memory state)
			(bf-state-pointer state)
			(+ (memory-get (bf-state-memory state)
				       (bf-state-pointer state))
			   1))))
     ((char= command #\[)
      (setf (bf-state-stack state)
	    (stack-push (bf-state-stack state)
			(bf-state-position state))))
     ((char= command #\])
      (if (> (memory-get (bf-state-memory state)
			 (bf-state-pointer state))
	     0)
	  (setf (bf-state-position state)
		(stack-peek (bf-state-stack state)))
	(setf (bf-state-stack state)
	      (stack-pop (bf-state-stack state)))))
     ((char= command #\,)
      (let ((character (char-code (read-char))))
	(setf (bf-state-memory state)
	      (memory-set (bf-state-memory state)
			  (bf-state-pointer state)
			  character))))
     ((char= command #\.)
      (write-char (code-char (memory-get (bf-state-memory state)
					 (bf-state-pointer state))))))
    (incf (bf-state-position state))
    state))

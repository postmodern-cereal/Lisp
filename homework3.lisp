(defclass world ()
  ((grid :accessor world-grid
	:initarg :grid)
  (agentX :accessor agentX
	    :initarg :agentX)
  (agentY :accessor agentY
	  :initarg :agentY)
  (agentD :accessor agentD
	  :initarg :agentD)
  (nops :accessor nops
	:initform 0)))

(defun breadth-first-search ())

(defmethod is-obstacle ((w1 world) X Y)
  (string= (aref (world-grid w1) X Y '0) 'O))


(defmethod find-successors-world ((w1 world) current)
  ;expand current: return all adjacent nodes
  ;(x, y, type)
  ;type is goal, obstacle, or space
  ;obstacles will not be returned
  (let ((successors nil))
  ;check each direction and add to the list if not illegal to do so
  ;left
  (if (and (>= (- (first current) 1) '0) (/= nil (is-obstacle w1 (- (first current) 1) (second current))))
      (setf successors (append successors '((- (first current) 1) (second current) (third current))))
      ())

  ;right
  (if (and (< (+ (first current) 1) 25) (/= (is-obstacle w1 (+ (first current) 1) (second current))))
      (setf successors (append successors '((+ (first current) 1) (second current) (third current))))
      ())
  
  ;up
  (if (and (>= (- (second current) 1) '0)  (/= (is-obstacle w1 (first current) (- (second current) 1))))
      (setf successors (append successors '((first current) (- (second current) 1) (third current))))
      ())

  ;down
  (if (and (< (+ (second current) 1) 25) (/= (is-obstacle w1 (first current) (+ (second current) 1))))
      (setf successors (append successors '((first current) (+ (second current) 1) (third current))))
      ())

  (write successors)))
      

;;;bfs (graph, root)
;;; create empty set s
;;; create empty queue q
;;; root.parent = NIL
;;; q.enqueue root
;;; while q not empty
;;;   curr = q.dequeue
;;;   if current is goal
;;;     return rurent
;;;   for ea noode n adj to curr
;;;     if n not in S
;;;      add n to s
;;;      n.parent = curr
;;;      q.enqueue(n)

(defclass world ()
  ((grid :accessor world-grid
	:initarg :grid)
  (agentX :accessor agent-X
	    :initarg :agentX)
  (agentY :accessor agent-Y
	  :initarg :agentY)
  (agentD :accessor agent-D
	  :initarg :agentD)
  (nops :accessor world-nops
	:initform 0)))

(defclass reflex-agent()
  ;percepts are a 2 element list
  ;first element tells what is in front
  ;second element tells if a bump happened and where
  ;element 0: B for blocked, C for clear
  ;element 1: L for left bump, R for right, F for front, etc
  ;element1: " " for no bump
  ((view :accessor agent-view
	   :initform 'Clear)
  (bump :accessor agent-bump
	:initform 'Clear)))

(defmethod senseUp ((w1 world) (r1 reflex-agent))
  (if (< (+ (agent-y w1) 1) 4)
      (write "yes")
      (write "false"))
  (if ((string= aref (world-grid w1) (agent-x w1) (+ (agent-y w1) 1) 0) 'O)
      (write "yes")
      (write 'no)))

(defmethod sense ((w1 world)  (r1 reflexAgent))
  (cond (((string= (agent-D w1) 'Up) and ((< (+ (agent-Y w1) 1) 4) or (string= (aref (world-grid w1) (agent-X w1) (+ (agent-Y w1) 1) 0) 'O)))
	    (setf (agent-view r1) 'Blocked))

	(((string= (agent-D w1) 'Up) and not ((< (+ (agent-Y w1) 1) 4) or (string= (aref (world-grid w1) (agent-X w1) (+ (agent-Y w1) 1) 0) 'O)))
	    (setf (agent-view r1) 'Clear))

	((string= (agent-D w1) 'Down)
	 (if((> (- (agent-Y w1) 1) 0) or (string= (aref (world-grid w1) (agent-X w1) (- (agent-Y w1) 1) 0) 'O))
	    (setf (agent-view r1) 'Blocked))
	 (setf (agent-view r1) 'Clear))

	((string= (agent-D w1) 'Left)
	 (if((> (- (agent-X w1) 1) 0) or (string= (aref (world-grid w1) (- (agent-X w1) 1) (agent-Y w1) 0) 'O))
	    (setf (agent-view r1 'Blocked))
	 (setf (first (agent-percept theAgent) 'Clear)))

	((string= (agent-D w1) 'Right)
	 (if((< (+ (agent-X w1) 1) 4) or (string= (aref (world-grid w1) (+ (agent-X w1) 1) (agent-Y w1) 0) 'O))
	    (setf (agent-view r1) 'Blocked))
	 (setf (agent-view r1) 'Clear)))

	(t "Something went wrong")))

(defmethod move-agent ((w1 world) type)
  ;type is L, R, F, B, C, or A
  ;Left, Right, Forward, Backward, Clockwise, Anticlockwise
  (cond ((string= type 'L) move-left(w1))
	((string= type 'R) move-right(w1))
	((string= type 'F) move-forward(w1))
	((string= type 'B) move-backward(w1))
	((string= type 'C) turn-clockwise(w1))
	((string= type 'D) turn-anticlockwise(w1))
	(t "Something went wrong")))

(defmethod move-left ((w1 world) (r1 reflexAgent))
  ;if agent-X -1 goes out of bounds, or impacts the same
  ;space as an obstacle, generate a hit-left bump
  ;otherwise, do the following:
      ;set the old agent position to contain nothing
      ;set the new agent position to contain an agent
      ;update the x value of the agent position with the new value
  (let ((testx (- 1 (agent-X w1))))
  (cond (((string= (aref (world-grid w1) testX (agent-Y w1) 0) 'O) or (> testX 0))
	 (setf (agent-bump r1) 'Left))
	(t (setf (aref (world-grid w1) (agent-X w1) (agent-Y w1) 0) 'Clear)
	   (setf (aref (world-grid w1) testX (agent-Y w1) 0) 'A)
	   (setf (agent-X w1) testX)))))

(defmethod move-right ((w1 world) (r1 reflexAgent))
  ;if agent-X -1 goes out of bounds, or impacts the same
  ;space as an obstacle, generate a hit-left bump
  ;otherwise, do the following:
      ;set the old agent position to contain nothing
      ;set the new agent position to contain an agent
      ;update the x value of the agent position with the new value
  (let ((testx (+ 1 (agent-X w1))))
  (cond (((string= (aref (world-grid w1) testX (agent-Y w1) 0) 'O) or (< testX 4))
	 (setf (agent-bump r1) 'Right))
	(t (setf (aref (world-grid w1) (agent-X w1) (agent-Y w1) 0) 'Clear)
	   (setf (aref (world-grid w1) testX (agent-Y w1) 0) 'A)
	   (setf (agent-X w1) testX)))))

(defmethod move-up ((w1 world) (r1 reflexAgent))
  ;if agent-X -1 goes out of bounds, or impacts the same
  ;space as an obstacle, generate a hit-left bump
  ;otherwise, do the following:
      ;set the old agent position to contain nothing
      ;set the new agent position to contain an agent
      ;update the x value of the agent position with the new value
  (let ((testY (+ 1 (agent-Y w1))))
  (cond (((string= (aref (world-grid w1) (agent-X w1) testY 0) 'O) or (< testY 0))
	 (setf (agent-bump r1) 'Up))
	(t (setf (aref (world-grid w1) (agent-X w1) (agent-Y w1) 0) 'Clear)
	   (setf (aref (world-grid w1) (agent-X w1) testY 0) 'A)
	   (setf (agent-Y w1) testY)))))

(defmethod move-down ((w1 world) (r1 reflexAgent))
  ;if agent-X -1 goes out of bounds, or impacts the same
  ;space as an obstacle, generate a hit-left bump
  ;otherwise, do the following:
      ;set the old agent position to contain nothing
      ;set the new agent position to contain an agent
      ;update the x value of the agent position with the new value
  (let ((testY (- 1 (agent-Y w1))))
  (cond (((string= (aref (world-grid w1) (agent-X w1) testY 0) 'O) or (> testY 0))
	 (setf (agent-bump) 'Down))
	(t (setf (aref (world-grid w1) (agent-X w1) (agent-Y w1) 0) 'Clear)
	   (setf (aref (world-grid w1) (agent-X w1) testY 0) 'A)
	   (setf (agent-Y w1) testY)))))

(defmethod turn-clockwise ((w1 world))
  (cond ((string= (agent-D w1) 'Up)
	 (setf (agent-D w1) 'Right))
	((string= (agent-D w1) 'Right)
	 (setf (agent-D w1) 'Down))
	((string= (agent-D w1) 'Down)
	 (setf (agent-D w1) 'Left))
	(t (setf (agent-D w1) 'Up))))

(defmethod turn-anticlockwise ((w1 world))
  (cond ((string= (agent-D w1) 'Up)
	 (setf (agent-D w1) 'Left))
	((string= (agent-D w1) 'Left)
	 (setf (agent-D w1) 'Down))
	((string= (agent-D w1) 'Down)
	 (setf (agent-D w1) 'Right))
	(t (setf (agent-D w1) 'Up))))

(defmethod no-op ((w1 world))
  (+ (world-nops w1) 1))

(defmethod initialize-world ((w1 world))
  (setf (aref (world-grid w1) (agent-X w1) (agent-Y w1) 0) 'A))

(defmethod draw-world ((w1 world))
  ;(drawline)
  (dotimes (i 5)
    (draw-line)
    (dotimes (j 5)
      (cond ((string= (aref (world-grid w1) i j 0) 'A)
	    (format t "| A "))
	    ((string= (aref (world-grid w1) i j 0) 'O)
	     (format t "| O "))
	    (t (format t "|   "))))
    (format t "|")
    (terpri))
  (draw-line))
      ;make sure am accessing stuff correctly
      ;(format t "| ~a |" (aref data i j 0)))))

(defun draw-line ()
  (dotimes (n 5)
    (write (first '(+)))
    (write (first '(---))))
  (write (first '(+)))
  (terpri))
  ;print top of table
  ;in loop: print row, then next top row

;write +--- for each element above
;write | before each element and  after
;write +-- for each elt below row

(defmethod reflexMove ((theAgent reflexAgent) (w1 world))
  ;create desired move
  ;return to world for assessment
  (cond ((= (second (agent-percept theAgent)) 'Clear)
	  (move-left w1 theAgent))
	 ((= (second (agent-percept theAgent)) 'Left)
	  (if (= first (agent-Percept theAgent) 'Blocked)
	      ;do a no-op, a.k.a, do nothing
	      ((no-op w1))
	  (move-up w1 theAgent)))
	 (t "Something went wrong")))
  
(defun runSimulation ()
  ;initialize world)
  ;loop and do the following)
  ;world does its stuff
  ;draw world
  ;check if done and correct
  (setq grid (make-array '(5 5 1) :element-type 'string :initial-element 'default))
  (setq w1 (make-instance 'world :grid grid :agentx 3 :agenty 3 :agentd 'Up))
  (initialize-world w1)
  ;insert some obstacles
  (setq r1 (make-instance 'reflex-agent))
  (draw-world w1)
  (loop
     ;return just takes you out of the loop, not out of the function
     (if (= (world-nops w1) 5)
	 (return))
     (sense w1 r1)
     (reflexMove r1 w1))
  ;evaluate performance
  )
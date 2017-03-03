(defun first2 (list)
  ;first ensure that list has 2 elements to return
  (if (<(length list) 2)
      nil
    ;return the first 2 elements
    (subseq list 0 2)))

(defun add1 (num)
  (+ 1 num))

(defun listadd1 (numList)
  ;ensure that list is not empty
  (if (<(length numList) 1)
      nil
      ;add 1 to first element, recursively call on rest of list
    (cons (1+ (first numList))
	  (listadd1 (rest numList)))))

(defun listadd2 (numList)
  (mapcar '1+ numlist))

(defun listadd3 (numList)
  (let (added) ;need an empty list to add to
    (dolist (element numList added)
      (setq added(cons (1+ element) added))) ;give name added to added + added number
   (reverse added)))

(defun flatten (list)
  (if (null list)
      nil
    (if (atom (first list)) ;atom indicates indivisible item, e.g a number or letter
	(cons (first list) (flatten (rest list))) ;it's an atom, and doesn't need to be split
      (append (flatten (first list)) (flatten (rest list)))))) ;it's a list and needs splitting

(defun last2 (list)
  (if (< (length list) 2)
      nil
    (subseq list (- (length list) 2) (length list))))

(defun my-reverse (list)
  (if (< (length list) 1)
      '() ;recursion bottoms out
    ;use list keyword to turn first element into a list
    ;and make the final product a single non dotted list
    (append (reverse (rest list)) (list (first list)))))

;I did not delete this function because I feel like I was onto something, but didn't quite figure it out
;(defun printTest (list)
  ;(format t "~{~a~%}" '(a b c 1 2 3))
  ;this won't be efficient but it will work
 ; (let ((nums ()))
  ;  (dotimes (n (length list))
     ; (setf nums (cons n nums)))
   ;(reverse nums)
   ; (setf nums(reverse nums))
    ;(format t "~{~@(~:r)~% ~}" nums)))
    ;(format t "~@{~:a~% ~}" nums)))

(defun printlist (list)
  (let ((num (length list)))
    (dotimes (n num)
      (format t "~@(~:r~) element: ~a~%" (+ n 1) (nth n list)))))

;(format t "~@(~:r~)" 123)

(defclass animal() 
  ((numlegs :accessor animal-legs :initform 4)
   (sound :initform "")))

(defclass cat (animal)
  ((sound :initform "meow")))

(defclass bird (animal)
  ((numlegs :initform 2)
   (sound :initform "tweet")))

(defmethod speak ((theAnimal animal))
  (slot-value theAnimal 'sound))

(defun copyfile (source destination)
  ;copy from source into destination
  (with-open-file (in source
		      :direction :input)
    (with-open-file (out destination
			 :direction :output
			 :if-exists :new-version
			 :if-does-not-exist :create)
      (loop with line
	   until (eql :eof (setq line (read-line in nil :eof)))
	   do(write-line line out)))))

(defun foo (a b &optional (c nil c-supplied-p))
  (list a b c c-supplied-p))

;this is an attempt at question 10 that doesn't work
;i honestly don't know how to make it work
(defun next (&optional (list nil list-supplied-p))
  (if (eql nil list-supplied-p)
      (progn (eval next)))
  (setq next '(car list)))
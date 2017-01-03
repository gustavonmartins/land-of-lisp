;performance optimizations from chapter 15
;pg 326 to 334
(load "dice-of-doom-v1-slow.lisp")

(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))



(let ((old-neighbors (symbol-function 'neighbors)) ;this overwritting is bad!
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
	(setf (gethash pos previous) (funcall old-neighbors pos)))))

(let ((old-game-tree (symbol-function 'game-tree))
	(previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
	(setf (gethash rest previous) (apply old-game-tree rest)))))

(let ((old-rate-position (symbol-function 'rate-position))	;pg 330
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
	(setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
	  (setf (gethash tree tab)
		(funcall old-rate-position tree player))))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
	     (cond ((zerop n) (append (reverse acc) lst))
		   ((null lst) (reverse acc))
		   (t (let ((cur-player (caar lst))
			    (cur-dice (cadar lst)))
			(if (and (eq cur-player player)
				 (< cur-dice *max-dice*))
			    (f (cdr lst)
			       (1- n)
			       (cons (list cur-player (1+ cur-dice)) acc))
			    (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

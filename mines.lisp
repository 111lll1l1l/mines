(defmacro defsfun (name vars exp)
  (let ((value (gensym)))
    `(progn
       (defun (setf ,name) (,value ,@vars)
         (setf ,exp ,value))
       (defun ,name ,vars
         ,exp))))

(defsfun mine-index (board i)
  (car (aref board i)))

(defsfun status-index (board i)
  (cadr (aref board i)))

(defun rand-board (n m num xy)
  (let* ((nm (* n m))
         (listboard (loop repeat nm collect (list 0 0))))
    (do ((i 0)
         (board (make-array nm :initial-contents listboard))
         (rand (random nm) (random nm)))
        ((>= i num) board)
      (unless (or (= rand xy)
                  (= (mine-index board rand) 9))
        (incf i)
        (setf (mine-index board rand) 9)))))

(defun decode-xy (n xy)
  (multiple-value-bind (x y) (truncate xy n)
    (list x y)))

(defun code-xy (n list-xy)
  (+ (* (car list-xy) n) (cadr list-xy)))

(defun in-board (n m xy)
  (destructuring-bind (x y) xy
    (if (or (< x 0) (< y 0) (>= x n) (>= y m)) nil t)))

(defun neighbors (n m xy)
  (destructuring-bind (x y) (decode-xy n xy)
    (mapcar #'(lambda (xy) (code-xy n xy))
            (remove-if-not #'(lambda (xy) (in-board n m xy))
                           (mapcar #'(lambda (d) (list (+ (car d) x)
                                                       (+ (cadr d) y)))
                                   '((-1 -1) (-1 0) (-1 1) (0 -1)
                                     (0 1) (1 -1) (1 0) (1 1)))))))

(defun new-board (n m num xy)
  (let ((board (rand-board n m num xy)))
    (list n m num board)))

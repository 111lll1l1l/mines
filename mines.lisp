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

(defun decode-xy (m xy)
  (multiple-value-bind (x y) (truncate xy m)
    (list x y)))

(defun code-xy (m list-xy)
  (+ (* (car list-xy) m) (cadr list-xy)))

(defun in-board (n m xy)
  (destructuring-bind (x y) xy
    (if (or (< x 0) (< y 0) (>= x n) (>= y m)) nil t)))

(defun neighbors (n m xy)
  (destructuring-bind (x y) (decode-xy m xy)
    (mapcar #'(lambda (xy) (code-xy m xy))
            (remove-if-not #'(lambda (xy) (in-board n m xy))
                           (mapcar #'(lambda (d) (list (+ (car d) x)
                                                       (+ (cadr d) y)))
                                   '((-1 -1) (-1 0) (-1 1) (0 -1)
                                     (0 1) (1 -1) (1 0) (1 1)))))))

(defun nu-board (n m board)
  (loop for i below (* n m)
        when (= (mine-index board i) 9) do
          (mapc #'(lambda (xy)
                    (unless (= (mine-index board xy) 9)
                      (incf (mine-index board xy))))
                (neighbors n m i))))

(defun new-board (n m num xy)
  (let ((board (rand-board n m num xy)))
    (nu-board n m board)
    (list n m num board)))

(defun print-board (board)
  (let ((n (car board)) (m (cadr board)) (bd (cadddr board)))
    (dotimes (i n)
      (dotimes (j m)
        (let* ((index (code-xy m (list i j)))
               (st (status-index bd index))
               (mi (mine-index bd index)))
          (cond
            ((= st 0) (princ #\+))
            ((= st 1) (princ #\f))
            ((= mi 9) (princ #\*))
            (t (princ mi)))
          (princ #\ )))
      (princ #\newline))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following Lisp code implements a basic algebra system. Vector algebra expressions can be expanded and simplified. ;;
;; 															 ;;
;; m+    ..  add two matrices												 ;;
;; m/s   ..  divide each matrix element by the scalar s									 ;;
;; m*v   ..  matrix vector product											 ;;
;; elt2  ..  access matrix element (row, column)									 ;;
;; m*    ..  3x3 matrix-matrix product											 ;;
;; det2  ..  determinant of 2x2 matrix
;; m2inv ..  invert 2x2 matrix
;; eps   ..  epsilon tensor (used in definition of cross product)							 ;;
;; cross ..  cross product of 3 vectors											 ;;
;; dotv  ..  dot product of 3 vectors											 ;;
;; simp  ..  simplify an expression (iterates 12 times by default)							 ;;
;; skew-sym-cross .. construct a skew symmetric matrix from 3 vector (used in quaternion conversion)			 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun v+ (a b)
  (loop for ra in a and rb in b collect
       `(+ ,ra ,rb)))

(defun v- (a b)
  (loop for ra in a and rb in b collect
       `(- ,ra ,rb)))

(defun elt2 (a i j)
  (elt (elt a i) j))

#+nil
(elt2
 '((1 2 3)
   (4 5 6)
   (7 8 9))
 0 2)



(defun m+ (a b)
  (loop for ra in a and rb in b collect
       (loop for ca in ra and cb in rb collect
	    `(+ ,ca ,cb))))
#+nil
(m+ '((1 2 3)
      (4 5 6)
      (7 8 9))
    '((1 0 0)
      (0 1 0)
      (0 0 1)))

(defun m/s (a s)
  (loop for ra in a collect
       (loop for ca in ra collect
	    `(/ ,ca ,s))))

(defun det2 (aa)
  (assert (= (length aa) 2))
  (assert (= (length (first aa)) 2))
  (let ((a (elt2 aa 0 0))
	(b (elt2 aa 0 1))
	(c (elt2 aa 1 0))
	(d (elt2 aa 1 1)))
    `(- (* ,a ,d) (* ,b ,c))))

(defun m2inv (aa)
  (assert (= (length aa) 2))
  (assert (= (length (first aa)) 2))
  (let ((a (elt2 aa 0 0))
	(b (elt2 aa 0 1))
	(c (elt2 aa 1 0))
	(d (elt2 aa 1 1)))
    (m/s (list (list d (- b))
	       (list (- c) a)) (det2 aa))))

#+nil
(simp :expr (m2inv (list (list 1 2)
	      (list 3 4))))


(defun m*v (a v)
  (assert (= (length (first a)) (length v)))
  (loop for i below (length a) collect
       `(+ ,@(loop for j below (length v) collect
		  `(* ,(elt2 a  i j) ,(elt v j))))))

#+nil
(m*v '((1 0 0)
       (0 1 0)
       (0 0 1))
     '(x y z))


(defun m* (a b)
  (assert (= (length (first a)) (length b)))
  (loop for i below (length (first a)) collect
       (loop for j below (length b) collect
	    `(+ ,@(loop for k below (length (first b)) collect
		       `(* ,(elt2 a i k)
			   ,(elt2 b k j)))))))

#+nil
(loop for i below 3 collect
     (loop for j below 3 collect
	  (elt2 
	   '((1 2 3)
	     (4 5 6)
	     (7 8 9))
	   i j)))

#+nil
(m* '((1 2 3)
      (4 5 6)
      (7 8 9))
    '((1 0 0)
      (0 1 0)
      (0 0 1)))



;; https://en.wikipedia.org/wiki/Levi-Civita_symbol
(defun eps (&rest rest)
  (cond
    ((member rest '((0 1 2) (1 2 0) (2 0 1)) :test #'equal)
     1)
    ((member rest '((2 1 0) (0 2 1) (1 0 2)) :test #'equal)
     -1)
    ((or (= (elt rest 0) (elt rest 1))
	 (= (elt rest 1) (elt rest 2))
	 (= (elt rest 2) (elt rest 0)))
     0)
    (t (break "unexpected input"))))


(defun cross (u v)
  (assert (= 3 (length u)))
  (assert (= 3 (length v)))
  (loop for i below 3 collect
       `(+ ,@(loop for j below 3 collect
		  `(+ ,@(loop for k below 3 collect
			     `(* ,(eps i j k) ,(elt u j) ,(elt v k))))))))
#+nil
(cross '(1 0 0) '(0 1 x))

(defun dotv (u v)
  (assert (= (length u) (length v)))
  `(+ ,@(loop for i below (length u) collect
	     `(* ,(elt u i) ,(elt v i)))))

#+nil
(dotv '(1 2 3) '(0 1 x))

(defun simplify-expr (expr)
  (cond
    ((null expr)
     0)
    ((eq '* (car expr))
     (cond ((member 0 (cdr expr)) ;; all products with zero are zero
	    0)
	   ((= 1 (length (cdr expr))) ;; (* 6 ) -> 6
	    (cadr expr))
	  
	   ((member 1 (cdr expr)) ;; remove 1 from products
	    `(* ,@(set-difference (cdr expr) (list 1))))
	   ((and (= 2 (length (cdr expr)))
		 (or (and (numberp (second expr)) 
			  (= (second expr) -1))
		     (and (numberp (third expr)) 
			      (= (third expr) -1)))) 
	    (cond ((and (numberp (second expr)) 
			(= (second expr) -1)) ;; (* -1 x) -> (- x)
		   `(- ,(third expr)))
		  ((and (numberp (third expr)) 
			      (= (third expr) -1)) ;; (* x -1) -> (- x)
		   `(- ,(second expr)))
		  (t (break "error"))))
	   (t
	    (let* ((numbers (remove-if #'(lambda (x) (= 1 x))
				       (remove-if #'(lambda (x) (not (numberp x))) (cdr expr)))) ;; all numbers except 1
		  (not-numbers (remove-if #'numberp (cdr expr)))
		  (terms (append not-numbers (list (reduce #'* numbers))))) ;; multiply out products
	      (if terms
		  `(* ,@terms)
		  0)))))
    ((eq '+ (car expr))
     (cond ((= 1 (length (cdr expr))) ;; (+ 1) -> 1
	    (car (cdr expr)))
	   ((member 0 (cdr expr)) ;; remove zeros
	    (let ((terms (set-difference (cdr expr) (list 0))))
	      (if terms
		  `(+ ,@terms)
		  0)))
	   (t
	    (let* ((numbers (remove-if #'(lambda (x) (not (numberp x))) (cdr expr)))
		   (not-numbers (remove-if #'numberp (cdr expr))) ;; sum all numbers
		   (terms (append not-numbers (list (reduce #'+ numbers))))) 
	      (when terms `(+ ,@terms))))))
    ((eq '- (car expr))
     (cond ((member 0 (cdr expr)) ;; zeros are occuring
	    (cond ((and (numberp (cadr expr))
			(= 0 (cadr expr)))
		   ;; first element is zero (- 0 323) -> (- 323)
		   (let ((terms (cddr expr)))
		     (if terms
			 `(- ,@terms)
			 0)))
		  (t
		   (destructuring-bind (op leading &rest rest) expr
		      ;; leave leading element in place
		    (let ((terms (set-difference rest (list 0))))
		      (if terms
			  `(- ,leading ,@terms)
			  `(- ,leading)))))))
	   ((and (= 1 (length (cdr expr))) ;; (- 5) -> -5
		 (numberp (cadr expr)))
	    (- (cadr expr)))
	   ((and (cdr expr)
		 (= 1 (length (cdr expr))) ;; (- (- x)) -> x
		 (cadr expr)
		 (listp (cadr expr))
		 (= 1 (length (cdr (cadr expr))))
		 (eq '- (car (cadr expr))))
	    (cadr (cadr expr)))
	   (t expr)))
    ((eq '/ (car expr))
     (cond ((member 0 (cdr expr)) ;; search zeros
	    (if (and (numberp (cadr expr))
		     (= 0 (car (cdr expr)))) ;; first element is zero
		0))
	   ((and (= 2 (length (cdr expr))) ;; division by 1, (/ x 1) -> x
		 (and
		  (numberp (second (cdr expr)))
		  (= 1 (second (cdr expr)))))
	    (first (cdr expr)))
	   ((and (= 2 (length (cdr expr ))) ;; division of two numbers (/ 4 3) -> 4/3
		 (numberp (second expr))
		 (numberp (third expr)))
	    (/ (second expr)
	       (third expr)))
	   (t expr)))
    
    (t expr)))

(defun skew-sym-cross (a)
  (let ((u (elt a 0))
	(v (elt a 1))
	(w (elt a 2))) 
    `((0 (- ,w) ,v)
      (,w 0 (- ,u))
      ((- ,v) ,u 0))))

#+nil
(skew-sym-cross '(1 2 3))

(defun simplify (exp)
  (if (atom exp)
      exp
      (simplify-expr (mapcar #'simplify exp))))

(defun simp (&key expr (n 12))
  (let* ((old-expr expr))
    (loop for i below n do
	 (setf old-expr expr
	       expr (simplify expr)))
    expr))


#+nil
(simp :n 100 :expr
      (let* ((a '(0 0 1))
	     (b (let* ((alpha (* (/ 180) pi 30))
		       (cx (sin alpha))
		       (cy 0)
		       (cz (cos alpha)))
		  `(,cx ,cy ,cz)))
	     (c (dotv a b))
	     (v (cross a b))
	     (sv (skew-sym-cross v))
	     (m (m+
		 (m+ '((1 0 0)
		       (0 1 0)
		       (0 0 1))
		     sv)
		 (m/s (m* sv sv)
		      `(+ 1 ,c)))))
	(m*v m b)))


#+nil
(defun rotate-system-so-axis-aligns-with-vector (vector sys-vec &key (axis '(0 0 1)) )
  ;; rotate a on b
 (let* ((a axis)
	(b vector)
	(c (dotv a b))
	(v (cross a b))
	(sv (skew-sym-cross v))
	(m (m+
	    (m+ '((1 0 0)
		  (0 1 0)
		  (0 0 1))
		sv)
	    (m/s (m* sv sv)
		 `(+ 1 ,c)))))
   (m*v m sys-vec)))


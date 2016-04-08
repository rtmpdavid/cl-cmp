;;;; cl-cmp.lisp

(in-package #:cl-cmp)

;;; "cl-cmp" goes here. Hacks and glory await!

(defparameter *preds* '(< <= = /= > >= ))
(defparameter *ops2-1* '(* /-))

(defparameter *ops2-2* '(+ -
			 max min
			 expt
			 gcd lcm
			 mod rem
			 logand logandc1 logandc2 logeqv logior lognand lognor logorc1 logorc2 logxor))

(defparameter *ops* nil)

(defmacro define-pred (name (arg1 arg2) &body body)
  `(progn
     (defun ,name (,arg1 ,arg2) ,@body)
     (unless (find ',name *preds*)
       (push ',name *preds* ))
     ',name))

(defmacro define-expr (name (arg1 arg2) &body body)
  `(progn
     (defun ,name (,arg1 ,arg2) ,@body)
     (unless (find ',name *ops2-1*)
       (push ',name *ops2-1* ))
     ',name))

(defmacro define-op (name (arg) &body body)
  `(progn
     (defun ,name (,arg) ,@body)
     (unless (find ',name *ops*)
       (push ',name *ops* ))
     ',name))

(define-op ! (n)
  (if (= n 1) 1
      (* n (! (1- n)))))

(defun exprp1 (s)
  (find s *ops2-1*))

(defun exprp2 (s)
  (find s *ops2-2*))

(defun predp (s)
  (find s *preds*))

(defun op-p (s)
  (find s *ops*))

(defparameter rules
  '(((op1 n) (e1 (op1 n)))
    ((op1 s) (e1 (op1 s)))
    
    ((s) (e1 (s)))
    ((n) (e1 (n)))

    ((e1 op2-1 e1) (e1 (op2-1 e1 e1)))
    ((e1 op2-2 e1) (e1 (op2-2 e1 e1)))
    
    ((ps e2 pc) (e2 e2))
    ((e2 op2 e2) (e2 (op2 e2 e2)))
    ((e1) (e2 (e1)))
    ((e2) (e3 (e2)))
    ((e3 cm2 e3 cm2) (e4 (cm2 e3 e3)) :apnd ((e3 ((2 e3))) (cm2 ((2 cm2)))))
    ((e3 cm2 e3) (e4 (cm2 e3 e3)))))

(defun match (expr rule)
  (loop
     with rest = expr
     for i from 0
     while (>= (length rest) (length (car rule)))
     if (loop
	   for term in (car rule)
	   for ex in rest
	   always (eq term (car ex)))
     do (return (list i rule))
     do (setf rest (cdr rest))))

(defun trans (rule expr trans)
  (if (listp trans)
      (loop
	 with table = (make-hash-table)
	 with rest = trans
	 for term = (car rest)
	 with pos = nil
	 while rest
	 if (atom term)
	 do (setf pos (position term rule :start (gethash term table 0)))
	 if pos
	 do (setf (gethash term table) (1+ pos))
	 and collect (cadr (nth pos expr))
	 else
	 collect  (cadr  (nth (1- (car term))
			      (remove-if-not
			       #'(lambda (a) (eq a (cadr term)))
			       expr :key #'car)))
	 end 
	 do (setf rest (cdr rest)))
      (cdr (nth (position trans rule) expr))))

(defun trans-expr (match expr)
  (let* ((s (car match))
	 (rule (car (cadr match)))
	 (pref (caadr (cadr match)))
	 (trans (cadadr (cadr match)))
	 (apnd)
	 (e (subseq expr s (+ s (length rule))))
	 (transformed (trans rule e trans)))
    (setf apnd (loop for a in (getf (cadr match) :apnd)
		  for apnd = (append (list (car a)) (trans rule e (cadr a)))
		  if (= 1 (length apnd)) collect apnd
		  else
		  appending (list apnd)
		  end
		    ))
    (list (append (list pref)
		(if (= 1 (length transformed)) transformed
		    (list transformed)))
	  apnd)))

(defun rep-rule (match expr)
  (let* ((new-sub (trans-expr match expr))
	 (len (length (car (cadr match))))
	 ;; (len-head (length (car new-sub)))
	 (new-tail (cdr (nthcdr (+ (car match) (1- len)) expr)))
	 (new-head (subseq expr 0 (car match)))
	 ;; (new-lst (remove t (replace expr new-sub :start1 (car match) :end1 (+ (car match) len))
	 ;; 		  :test #'(lambda (a b) (or a b)) :start (1+ (car match)) :end (+ (car match) len)))
	 )
    ;; (print (list match  (elt expr (car match))))
    (append new-head
	    (list (car new-sub))
	    (cadr new-sub)
	    new-tail)))

(defun pred-expand (body)
  (let ((expr body))
    (loop for done = t
       do (setf expr (loop for tok in expr
			for procd = (cond
				      ((numberp tok) (list 'n tok))
				      ((predp tok) (list 'cm2 tok))
				      ((exprp1 tok) (list 'op2-1 tok))
				      ((exprp2 tok) (list 'op2-2 tok))
				      ((op-p tok) (list 'op1 tok))
				      ((eq '[ tok) (list 'ps))
				      ((eq '] tok) (list 'pc))
				      ((symbolp tok) (list 's tok))
				      (t nil))
			if procd
			collect procd and
			do (setf done nil)
			else
			collect tok
			end))
	 
       until done)
    (format t "Parsed: ~a~%" expr)
    (loop for match = (loop
			 for rule in rules
			 for match = (match expr rule)
			 until match
			 finally (return match))
       while match
       ;; do (print expr)
       do (setf expr (rep-rule match expr))
       ;; do (print expr)
       finally (return expr))))

(defmacro cmp (&body body)
  `',(pred-expand body))

; (_jif then else test)
; (_jpair then else ob)
; (_jtype_x then else ob)
; (_bind_
;
; (jtrue T E False) -> E
; (jtrue T E _) -> (T _)
; (jbtype-sn T E #[s v1 .. vn]) -> (T v1 .. vn)
; (jbtype-sn T E _) -> (E _)
; (jt-t T E O) -> (T O) | (E O)
;
; object types
;	 0 = call, 1 = pair, 2 = symbol
;

(define (op-input node)
	(let*
		((fd (cdr node))
		 (char (read-char fd)))
		(set-car! node op-i)
		(if (eof-object? char)
			(begin
				(set-cdr! node (list))
				(exit 0))
			(set-cdr! node
				(vector 1
					(char->integer char)
					(cons op-input fd))))
		(cdr node)))

(define (print . args)
	(for-each display args)
	(newline))

(define-syntax update-cdr!
	(syntax-rules ()
		((update-cdr! obj val)
			(set-cdr! obj (evaluate val)))))
					
(define (evaluate node)
	(if (pair? node)
		(if (procedure? (car node))
			((car node) node)
			(let ((a (evaluate (car node))))
				(set-car! node a)
				(a node)))
		node))

; return an evaluated value 
(define (op-ret node) (cdr node))

(define (op-ix node) 
	(let ((val (cdr node)))
		(if (pair? val)
			(begin
				(update-cdr! node (evaluate val))
				(set-car! node op-ret)
				(cdr node)
				;(evaluate val)
				)
			(begin
				(set-car! node op-ret)
				val))))

(define (op-i an) 
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(set-car! an op-ret)
	(cdr an))

(define (op-y node)
	(set-car! node (evaluate (cdr node)))
	(set-cdr! node node)
	((car node) node))

;(define (op-y node)
;	(let ((x (cdr node)))
;		(if (pair? x)
;			(set-car! node (evaluate x))
;			(set-car! node x))
;		;(set-cdr! node (cons op-y x))
;		(set-cdr! node node)
;		(evaluate node)))
;	;(set-car! (cdr node))
;	;(set-cdr! node node)
	

; (C a b c) = pass left only = ((a c) b) 
; (B a b c) = pass right only = (a (b c))
(define (op-s an)
	(lambda (bn)
		(if (pair? (cdr an)) (set-cdr! an (evaluate (cdr an))))
		(if (pair? (cdr bn)) (set-cdr! bn (evaluate (cdr bn))))
		(lambda (cn)
			(let* 
				((a (cdr an))
				 (b (cdr bn))
				 (c (cdr cn))
				 (new-a (cons a c)))
				(set-car! cn new-a)
				(set-cdr! cn (cons b c))
				; (evaluate cn) 
				((a new-a) cn)
				))))

; (S2 a b c d) = ((a (b d)) (c d))
(define (op-s2 an)
	(lambda (bn)
		(lambda (cn)
			(lambda (dn)
				(if (pair? (cdr an)) (set-cdr! an (evaluate (cdr an))))
				(let*
					((a (cdr an))
					 (b (cdr bn))
					 (c (cdr cn))
					 (d (cdr dn))
					 (anew (cons a (cons b d))))
					(set-car! dn anew)
					(set-cdr! dn (cons c d))
					;(evaluate dn)
					((a anew) dn)
					)))))

; (C2 a b c d) = ((a (b d)) c)
(define (op-c2 an)
	(lambda (bn)
		(lambda (cn)
			(lambda (dn)
				(if (pair? (cdr an)) (set-cdr! an (evaluate (cdr an))))
				(let*
					((a (cdr an))
					 (b (cdr bn))
					 (c (cdr cn))
					 (d (cdr dn))
					 (anew (cons a (cons b d))))
					;(set-car! dn (cons a (cons b d)))
					;(set-cdr! dn c)
					(set-car! dn anew)
					(set-cdr! dn c)
					;(evaluate dn)
					((a anew) dn)
					)))))


; (B2 a b c d) = ((a b) (c d))
(define (op-b2 an)
	(lambda (bn)
		(lambda (cn)
			(lambda (dn)
				(if (pair? (cdr an)) (set-cdr! an (evaluate (cdr an))))
				(let
					((a (cdr an))
					 (b (cdr bn))
					 (c (cdr cn))
					 (d (cdr dn)))
					(set-car! dn a)
					(set-cdr! dn (cons b (cons c d)))
					(a dn)
					)))))

(define (op-c an)
	(lambda (bn)
		(lambda (cn)
			(if (pair? (cdr an)) (set-cdr! an (evaluate (cdr an))))
			(let* 
				((a (cdr an))
				 (b (cdr bn))
				 (c (cdr cn))
				 (new-a (cons a c)))
				(set-car! cn (cons a c))
				(set-cdr! cn b)
				; (evaluate cn) ==
				((a new-a) cn)
				))))

(define (op-b an)
	(lambda (bn)
		(lambda (cn)
			(if (pair? (cdr an)) (set-cdr! an (evaluate (cdr an))))
			(let 
				((a (cdr an))
				 (b (cdr bn))
				 (c (cdr cn)))
				(set-car! cn a)
				(set-cdr! cn (cons b c))
				; (evaluate cn) ==
				(a cn)
				))))

(define (op-k an)
	(lambda (bn) 
		(let ((val (cdr an)))
			(if (pair? val)
				(begin
					(update-cdr! an val)
					(cdr an))
				val))
		;(evaluate (cdr an))
		))

(define (op-if tn)
	(lambda (an)
		(lambda (bn)
			(let ((test (cdr tn)))
				(if (pair? test)
					(update-cdr! tn test))
				(if (cdr tn)
					(set-cdr! bn (cdr an)))
				(set-car! bn op-i)
				(evaluate bn)
				))))

(define (op-nilp an)
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(null? (cdr an)))

(define (op-fixnum? an)
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(number? (cdr an)))

(define (op-pair? an)
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(and (vector? (cdr an))
		(eq? (vector-ref (cdr an) 0) 1)))

(define (op-eq? an)
	(if (pair? (cdr an)) 
		(update-cdr! an (evaluate (cdr an))))
	(lambda (bn)
		(if (pair? (cdr bn)) 
			(update-cdr! bn (evaluate (cdr bn))))
		(eq? (cdr an) (cdr bn))))


; symbol type is 2 
(define (op-symbol? an)
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(and (vector? (cdr an))
		(eq? (vector-ref (cdr an) 0) 2)))

(define (op-cons an)
	(lambda (bn)
		(vector 1 (cdr an) (cdr bn))))

; 
; ((mknode Type Size) a0 ... an) -> #(Type a0 ... an)

(define (halt . reason)
	(apply print (cons "fvm: " reason))
	(exit 1))

(define (op-car an)
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(let ((ob (cdr an)))
		(if (vector? ob)
			(let ((val (vector-ref ob 1)))
				(if (pair? val)
					(let ((val (evaluate val)))
						(vector-set! ob 1 val)
						val)
					val)
				;(evaluate val)
				)
			(halt "Bad car pair: " ob))))

(define (op-cdr an)
	(if (pair? (cdr an))
		(update-cdr! an (cdr an)))
	(let ((ob (cdr an)))
		(if (vector? ob)
			(let ((val (vector-ref ob 2)))
				(if (pair? val)
					(let ((val (evaluate val)))
						(vector-set! ob 2 val)
						val)
					val)
				;(evaluate val)
				)
			(halt "Bad cdr pair: " ob))))

(define (num-op op)
	(lambda (an)
		(if (pair? (cdr an))
			(update-cdr! an (cdr an)))
		(cond
			((number? (cdr an))
				(lambda (bn)
					(if (pair? (cdr bn))
						(update-cdr! bn (cdr bn)))
					(cond
						((number? (cdr bn))
							(set-car! bn op-ret)
							(set-cdr! bn (op (cdr an) (cdr bn)))
							(cdr bn))
						(else 
							(print "not number in number op")))))
			(else
				(halt "bad number")))))

(define op-add (num-op +))
(define op-mul (num-op *))
(define op-neq (num-op =))
(define op-sub (num-op -))
(define op-div (num-op (lambda (a b) (floor (/ a b)))))
(define op-mod (num-op modulo))
(define op-< (num-op <))
(define op-> (num-op >))

; (probe b Y) = Y, sending byte X 
(define (op-probe an)
	(lambda (bn)
		(let ((val (evaluate (cdr an))))
			(if (and (number? val) (< val 256))
				(lvm-emit-err val)
				(print val)))
		(evaluate (cdr bn))))

(define (op-mksym an)
	(set-car! an op-ret)
	(set-cdr! an (vector 2 (cdr an)))
	(cdr an))

(define (unary-constructor type)
	(if (= type 2)
		op-mksym
		(halt "no such unary constructor: " type)))

(define app-constructor
	(lambda (an)
		(lambda (bn)
			(set-car! bn (cdr an))
			(evaluate bn))))

(define (binary-constructor type)
	(if (= type 0)
		app-constructor
		(halt "no such binary constructor: " type)))

(define op-mkapp app-constructor)

(define (make-constructor size type)
	(cond	
		((= size 1) 	
			(unary-constructor type))
		((= size 2)
			(binary-constructor type))
		(else 
			(error "No this sized constructors yet: " size))))

(define (literal x) x)

(define prims
	(vector
		; 0       1         2        3        4       5         6       7        8         9
		op-s		op-k 		op-i 		op-b 		op-c 		op-s2 	op-c2		op-b2 	op-y 		op-y	; 0-9
		op-i		op-i 		op-i 		op-i 		op-i 		op-i 		op-i		op-i 		op-i 		op-i	; 10-19
		op-cons  op-car   op-cdr   op-i		op-i		op-i		op-i		op-i		op-i		op-i	; 20-29
		op-add	op-sub	op-mul	op-div	op-mod	op-neq	op->		op-<		op-i		op-i	; 30-39
		op-if	 op-nilp op-fixnum? op-symbol? op-eq? op-probe 	op-mksym	op-y 		op-pair?	op-mkapp	; 40-49
		))

(define (op-prim an)
	(let ((val (evaluate (cdr an))))
		(if (number? val)
			(vector-ref prims val)
			(halt "Bad primitive: " val))))

(vector-set! prims 47 op-prim)

(define combinators
	`((#\k . ,op-k) 
	  (#\+ . ,op-add)
	  (#\* . ,op-mul)
	  (#\- . ,op-sub)
	  (#\= . ,op-neq)
	  (#\> . ,op->)
	  (#\/ . ,op-div)
	  (#\% . ,op-mod)
	  (#\< . ,op-<)
	  (#\i . ,op-i)
	  (#\s . ,op-s)
	  (#\c . ,op-c)
	  (#\b . ,op-b)
	  (#\y . ,op-y)
	  (#\o . ,op-s2)
	  (#\m . ,op-c2)
	  (#\n . ,op-b2)
	  (#\I . ,op-if)
	  (#\C . ,op-cons)
	  (#\A . ,op-car)
	  (#\D . ,op-cdr)
	  (#\2 . ())
	  (#\3 . #t)
	  (#\4 . #f)
	  (#\N . ,op-nilp)	
	  (#\U . ,op-fixnum?)
	  (#\P . ,op-pair?)
	  (#\S . ,op-symbol?)
	  (#\@ . ,op-probe)
	  (#\E . ,op-eq?)
	  (#\$ . ,op-prim)
	  ))

; (if-pair T E (cons a b)) = (T a b)
; (if-pair T E _) = (E _)

(define (pair-dispatcher tn)
	(lambda (en)
		(lambda (on)
			(if (pair? (cdr on))
				(set-cdr! on (evaluate (cdr on))))
			(let ((obj (cdr on)))
				(if (and (vector? obj) (= 1 (vector-ref obj 0)))
					(begin
						(set-cdr! on (vector-ref obj 2))
						(set-car! on 
							(cons (cdr tn) (vector-ref obj 1)))
						(evaluate on))
					(begin
						(set-car! on (cdr en))
						(evaluate on)))))))

(define (make-dispatcher type amount)
	(if (and (= type 1) (= amount 2))
		pair-dispatcher
		(halt "fvm: no suitable dispatcher")))

(define magic (string->list "LEAF"))

(define (seek-magic port left)
	(if (null? left)
		(begin
			;(print "fvm: good magic")
			port)
		(let ((this (read-char port)))
			(cond
				((eof-object? this)
					(halt "fvm: found no magic"))
				((eq? this (car left))
					(seek-magic port (cdr left)))
				(else
					(seek-magic port magic))))))

(define (load-code port)
	(let ((this (read-char port)))
		;(print " got " this)
		(cond
			((eq? this #\newline)
				(load-code port))
			((eq? this #\space)
				(let* 
					((rator (load-code port))
					 (rand (load-code port)))
					(cons rator rand)))
			((eq? this #\Q)	; mk size type
				(let 
					((size (- (char->integer (read-char port)) 48))
					 (type (- (char->integer (read-char port)) 48)))
					(make-constructor size type)))
			((eq? this #\W) ; (dispatch type amount [then else obj])
				(let 
					((type (- (char->integer (read-char port)) 48))
					 (amount (- (char->integer (read-char port)) 48)))
					(make-dispatcher type amount)))
			((assq this combinators) =>
				(lambda (this) (cdr this)))
			((eq? this #\l)
				(let ((a (read-char port)))
					(char->integer a)))
			(else
				(print "Error while loading, unknown char: " (char->integer this))))))

(define (lvm-emit byte)
	(display (integer->char byte)))

(define (lvm-emit-err byte)
	(for-each
		(lambda (byte)
			(display (integer->char byte)))
		(list 27 91 51 49 109 byte 27 91 48 109)))


(define (io-trampoline code)
	(set! root code)
	(let ((value (evaluate code)))
		(if (vector? value)
			(let ((head (evaluate (vector-ref value 1))))
				(if (and (number? head) (< head 256) (>= head 0))
					(begin
						; (print "lvm: io trampoline emitting " head)
						(lvm-emit head)
						(io-trampoline (vector-ref value 2)))
					(begin
						(print "lvm: io trampoline halting on " value)
						value))))))

(define root #f)

(define (run path)
	;(print "fvm: " path)
	(let* 
		((port (open-input-file path))
		 (port (seek-magic port magic))
		 (code (load-code port)))
		(io-trampoline
			(cons code 
				(cons op-input 
					(current-input-port))))))

(let ((val (run (cadr (command-line-arguments)))))
	;(print "fvm: -> " val)
	(exit val))


; -*- scheme -*-

(define-module (aquarius aqvm))

(use-modules (ice-9 rdelim))

(define-public (aqvm code)
  (new-vm 1000)
  (run-vm code))

(define-public (new-vm size)
  (set! mem (make-array 0 size))
  (set! instr-table (make-instr-table))
  )

(define-public (add-instr name handler)
  (hashq-create-handle! instr-table name handler)
)

(define-public (run-vm code)
  (set! fp 0)
  (set! sp (car (array-dimensions mem)))
  (set! pc 0)
  (array-set! mem '(call 2) 0)
  (array-set! mem '(stop) 1)
  (copy-code code 2)
  (while (not (eq? (car (array-ref mem pc)) 'stop))
	 (exec-single-instr)))
;	 (execute-instruction)))

(define fp 0)
(define sp 0)
(define pc 0)
(define mem '())
(define instr-table '())

(define (exec-single-instr)
  (let ((instr (array-ref mem pc)))
    (set! pc (+ 1 pc))
    (primitive-eval instr)))

(define (execute-instruction)
  (let* ((instr (array-ref mem pc))
	 (exec (hashq-ref instr-table (car instr))))
    (set! pc (+ 1 pc))
    (exec (cdr instr))))

(define (make-instr-table)
  (let ((h (make-hash-table 127)))
    (hashq-create-handle! h 'begin-frame begin-frame)
    (hashq-create-handle! h 'end-frame end-frame )
    (hashq-create-handle! h 'return return )
    (hashq-create-handle! h 'call call )
    (hashq-create-handle! h 'push push-immediate )
    (hashq-create-handle! h 'read read-integer )
    (hashq-create-handle! h 'pop-fp pop-fp )
    (hashq-create-handle! h 'push-fp push-fp )
    (hashq-create-handle! h 'eq compare-eq )
    (hashq-create-handle! h 'ne compare-ne )
    (hashq-create-handle! h 'gt compare-gt )
    (hashq-create-handle! h 'lt compare-lt )
    (hashq-create-handle! h 'ge compare-ge )
    (hashq-create-handle! h 'le compare-le )
    (hashq-create-handle! h 'bne branch-not-zero )
    (hashq-create-handle! h 'beq branch-zero )
    (hashq-create-handle! h 'add arith-add )
    (hashq-create-handle! h 'sub arith-sub )
    (hashq-create-handle! h 'jump jump )
    (hashq-create-handle! h 'write write-integer )
    (hashq-create-handle! h 'newline write-newline )
    h)
)

(define (push x) 
  (set! sp (- sp 1))
  (array-set! mem x sp))

(define (pop)
  (set! sp (+ sp 1))
  (array-ref mem (- sp 1)))

(define (begin-frame)
  (push fp)
  (set! fp sp))

(define (end-frame)
  (set! sp fp)
  (set! fp (pop)))

(define (return)
  (set! pc (pop)))

(define (call target)
  (push pc)
  (set! pc target))

(define (push-immediate x)
  (push x))

(define (read-integer)
  (push (string->number (read-line))))

(define (pop-fp offset)
  (array-set! mem (pop) (- fp offset)))

(define (push-fp offset)
  (push (array-ref mem (- fp offset))))

(define (compare-eq)
  (stack-compare eq?))

(define (compare-ne)
  (stack-compare (lambda (x y) (not (eq? x y)))))

(define (compare-gt)
  (stack-compare >))

(define (compare-lt)
  (stack-compare <))

(define (compare-ge)
  (stack-compare >=))

(define (compare-le)
  (stack-compare <=))

(define (stack-compare op)
  (let ((right (pop))
	(left (pop)))
    (if (op left right) (push 1) (push 0))))

(define (branch-not-zero dest)
  (if (not (eq? (pop) 0)) (set! pc dest)))

(define (branch-zero dest)
  (if (eq? (pop) 0) (set! pc dest)))

(define (arith-add)
  (arith-op +))

(define (arith-sub)
  (arith-op -))

(define (arith-op op)
  (let ((right (pop))
	(left (pop)))
    (push (op left right))))

(define (jump dest)
  (set! pc dest))

(define (write-integer)
  (display (pop)))

(define (write-newline)
  (display "\n"))

(define (copy-code code start)
  (pass2 code start (pass1 code start '())))

(define (pass1 code start labels)
  (if (null? code) labels
      (let ((curr (car code)))
	(if (eq? (car curr) 'label)
	    (pass1 (cdr code) start (cons (list (cadr curr) start) labels))
	    (pass1 (cdr code) (+ 1 start) labels)))))

(define (pass2 code start labels)
  (if (null? code) '()
      (let* ((curr (car code))
	     (instr (if (label-arg? (car curr))
			(list (car curr) (get-label (cadr curr) labels))
			curr)))
	(if (eq? (car curr) 'label)
	    (pass2 (cdr code) start labels)
	    (begin
	      (array-set! mem instr start)
	      (pass2 (cdr code) (+ 1 start) labels))))))


(define (label-arg? item)
  (or (eq? item 'branch-not-zero) (eq? item 'branch-zero)
      (eq? item 'jump) (eq? item 'call)))

(define (get-label item labels)
  (car (assq-ref labels item)))

; (define (optimise code)
;   (if (null? code) '()
;       (let* ((top (car code))
; 	     (top-instr (car top))
; 	     (top-args (cdr top))
; 	     (rest (optimise (cdr code))))
; 	(if (push? top-instr)
; 	    (let ((match (pull-pop rest '())))
; 	      (if (null? (car match)) (cadr match)
; 		  (cons (make-move top (car match))
; 			(cadr match))))
; 	    (cons top rest)))))

; (define (push? instr)
;   (or (eq? instr 'push) (eq? instr 'push-fp)))

; (define (pop? instr)
;   (or (eq? instr pop-fp)))

; (define (make-move i-push i-pop)
;   (list 'move (to-source i-push) (to-dest i-pop)))

; (define (to-source instr)
;   (if (eq? (car instr) 'push-immediate


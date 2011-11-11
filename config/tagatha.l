; -*- scheme -*-

(define (eval-list list)
  (if (null? list) '() 
    (begin (display (car list)) (newline)
	   (primitive-eval (car list)) (eval-list (cdr list)))))

(define (run-tagatha source-file destination-file)
  (eval-list (read (open-input-file source-file)))
  (generate-code destination-file))

(define storage-unit 8)
(define word-size 16)
(define address-size 16)
(define max-type-storage 256)
(define short-integer-size 8)
(define integer-size 16)
(define long-integer-size 32)
(define null-address 0)
(define default-bit-order 'high-order-first)
(define default-byte-order 'low-order-first)
(define default-alignment 'unaligned)
(define default-stack-growth 'negative)
(define data-registers 4)
(define frame-pointer 4)
(define stack-pointer 5)

(define (c64)
  (set! storage-unit 8)
  (set! word-size 16)
  (set! address-size 16)
  (set! max-type-storage 256)
  (set! short-integer-size 8)
  (set! integer-size 16)
  (set! long-integer-size 32)
  (set! null-address 0)
  (set! default-bit-order 'high-order-first)
  (set! default-byte-order 'low-order-first)
  (set! default-alignment 'unaligned)
  (set! default-stack-growth 'negative)
  (set! data-registers 4)
  (set! frame-pointer 4)
  (set! stack-pointer 5)
)

(define (pdp11)
  (set! storage-unit 8)
  (set! word-size 16)
  (set! address-size 16)
  (set! max-type-storage 256)
  (set! short-integer-size 8)
  (set! integer-size 16)
  (set! long-integer-size 32)
  (set! null-address 0)
  (set! default-bit-order 'high-order-first)
  (set! default-byte-order 'low-order-first)
  (set! default-alignment 'align-to-word)
  (set! default-stack-growth 'negative)
  (set! data-registers 5)   ; r0 .. r4
  (set! frame-pointer 5)    ; r5 used for frame pointer
  (set! stack-pointer 6)    ; r6 conventionally stack
)

(define previous-frames '())
(define current-frame '())

(define (begin-frame)
  (start-frame-offset)
  (set! previous-frames (cons current-frame previous-frames))
  (set! current-frame '()))

(define (stack-variable size)
  (set! current-frame (cons (next-frame-offset size) current-frame)))

(define (allocate-stack)
  (reserve-local-space (frame-size))
  (set! current-frame (list->vector (reverse current-frame))))

(define (end-frame)
  (restore-local-space (frame-size))
  (set! current-frame (car previous-frames))
  (set! previous-frames (cdr previous-frames))
  )

(define (move-integer-to-temp size val tmp)
  (m-int-temp size val (get-temporary tmp)))

(define (move-stack-variable-to-temp size offset tmp)
  (m-frame-temp size (get-stack-variable offset) (get-temporary tmp)))

(define (move-temp-to-stack-variable size tmp index)
  (m-temp-frame size (get-temporary tmp) (get-stack-variable index)))

(define (apply size op left right result)
  (apply-op size (get-temporary left) op (get-temporary right)
	    (get-temporary result)))

(define (branch-zero src dest)
  (branch-zero-temp-label (get-temporary src) (get-label dest)))

(define (branch-not-zero src dest)
  (branch-not-zero-temp-label (get-temporary src) (get-label dest)))

(define (jump dest)
  (jump-to (get-label dest)))

(define (label name) (make-label (get-label name)))

(define (get-stack-variable index)
  (vector-ref current-frame index))

(define name-list (make-hash-table 439))

(define (get-from-name-list name constructor)
  (let ((h (hashq-ref name-list name)))
    (if h h
	(let ((h2 (constructor)))
	  (hashq-set! name-list name h2)
	  h2))))
  
(define (get-temporary tmp) (get-from-name-list tmp next-temporary))

(define (get-label name) (get-from-name-list name next-label))

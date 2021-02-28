#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file


;; put your code below

;-----------------------
; Problem #1
;-----------------------
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;-----------------------
; Problem #2
;-----------------------
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


;-----------------------
; Problem #3
;-----------------------
(define (list-nth-mod xs n)
  (cond [(empty? xs) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;-----------------------
; Problem #4
;-----------------------
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([cell (s)])
        (cons (car cell) (stream-for-n-steps (cdr cell) (- n 1))))))

;-----------------------
; Problem #5
;-----------------------
(define funny-number-stream
  (letrec ([m (lambda (x) (if (= (remainder x 5) 0) (- x) x))]
           [f (lambda (x) (cons (m x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;-----------------------
; Problem #6
;-----------------------
(define dan-then-dog
  (letrec ([first (lambda() (cons "dan.jpg" second))]
           [second (lambda() (cons "dog.jpg" first))])
    first))

;-----------------------
; Problem #7
;-----------------------
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (let([x (s)])
                         (cons (cons 0 (car x)) (lambda() (f (cdr x))))))])
    (lambda () (f s))))
                             
;-----------------------
; Problem #8
;-----------------------
(define (cycle-lists xs ys)
  
  (define (to-stream xs)
    (letrec ([stream (lambda (xs n) (cons (list-nth-mod xs n) (lambda () (stream xs (+ n 1)))))])
      (lambda () (stream xs 0))))
  
  (define (combine sx sy)
    (let ([x (sx)]
          [y (sy)])
      (cons (cons (car x) (car y)) (lambda() (combine (cdr x) (cdr y))))))          
  
  (lambda () (combine (to-stream xs) (to-stream ys))))
       
;-----------------------
; Problem #9
;-----------------------
(define (vector-assoc v vec)
  (define (iter vec n)
    (if (>= n (vector-length vec))
        #f
        (let([x (vector-ref vec n)])
          (if (and (pair? x) (equal? (car x) v))
              x
              (iter vec (+ n 1))))))
  (iter vec 0))


;-----------------------
; Problem #10
;-----------------------
(define (cached-assoc xs n)
  (let*([cache (make-vector n #f)]
        [slot 0]
        [update-cache (lambda (miss) 
                        (if (not miss) miss (begin (vector-set! cache slot miss)
                                        (if (= (+ slot 1) n)
                                            (set! slot 0)
                                            (set! slot (+ slot 1))) miss)))]
        [from-cache (lambda (v) (vector-assoc v cache))]
        [from-list (lambda (v) (update-cache (assoc v xs)))])
    (lambda (v) (let([hit (from-cache v)])
                  (if hit hit (from-list v))))))
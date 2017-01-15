
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride)))) 

; 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (cond 
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

; 4
(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (let ([sp (s)]) (cons (car sp) (stream-for-n-steps (cdr sp) (- n 1))))))

; helper function wich takes initial stream element and function to get the next element
; used un 5 and 6
(define (make-stream init next)
  (cons init (lambda () (make-stream (next init) next))))

; 5
(define (funny-number-stream)
  (make-stream 1 (lambda (n)
                   (let ([n (abs n)])
                     (if (= (remainder n 5) 4) ; (n % 5 = 4) => ((n + 1) % 5 = 0) 
                         (- (+ n 1))
                         (+ n 1))))))

; 6
(define (dan-then-dog)
  (let ([dan "dan.jpg"]
        [dog "dog.jpg"])
    (make-stream dan (lambda (x) (if (equal? x dan) dog dan)))))

; 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) 
                (let ([ps (s)])
                  (cons (cons 0 (car ps)) (lambda () (f (cdr ps))))))])
    (lambda () (f s))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons 
                 (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [vector-assoc-rec (lambda (pos)
                           (if (< pos l)
                               (let ([p (vector-ref vec pos)])
                                 (if (and (pair? p) (equal? v (car p))) 
                                     p 
                                     (vector-assoc-rec (+ pos 1))))                                
                               #f))])
                           (vector-assoc-rec 0)))

; 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n)]
           [slot 0]
           [f (lambda (v)
                (let* ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([ans (assoc v xs)])
                        (if ans
                            (begin
                              (vector-set! memo slot ans)
                              (set! slot (remainder (+ slot 1) n))
                              ans)
                            #f)))))])
    f))

; 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
    (let ([max e1]
          [th (lambda () e2)])
      (letrec ([while (lambda ()
                      (if (>= (th) max)
                          #t
                          (while)))])
    (while)))]))
                          
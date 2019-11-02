#lang racket

; Warm Up

; 1.
(define (my-length lst)
  (cond [(empty? lst) 0]
        [else (+ 1 (my-length (rest lst)))]))

; 2.
(define (onion n)
  (cond [(< n 0) (error ". . onion: n negative")]
        [(> n 0) (list (onion (- n 1)))]
        [(= n 0) '()]))

; 3.
(define (my-last lst)
  (cond [(empty? lst) (error ". . my-last: empty list")]
        [(empty? (rest lst)) (first lst)]
        [else (my-last (rest lst))]))

; 4.
(define (double-up lst)
  (foldr (lambda (next accum) (cons next (cons next accum))) '() lst))

; 5.
(define (swap-ends lst)
  (cond [(empty? lst) lst]
        [(empty? (rest lst)) lst]
        [else
         (let* ([first-item (first lst)]
                [rest-reversed (foldl cons '() (rest lst))]
                [last-item (first rest-reversed)]
                [mid-reversed (rest rest-reversed)])
           (cons last-item (foldl cons '() (cons first-item mid-reversed))))]))

; 6.
(define (softmax lst)
  (cond [(empty? lst) (error ". . softmax: lst is empty")]
        [else
         (let ([exponent-sum (foldl (lambda (next accum) (+ accum (exp next))) 0 lst)])
           (map (lambda (item) (/ (exp item) exponent-sum)) lst))]))

; 7.
(define (member-fn pred? lst)
  (cond [(empty? lst) #f]
        [(pred? (first lst)) #t]
        [else (member-fn pred? (rest lst))]))

(define (member val lst)
  (member-fn (lambda (item) (equal? item val)) lst))

(define (unique? lst)
  (cond [(empty? lst) #t]
        [(member (first lst) (rest lst)) #f]
        [else (and #t (unique? (rest lst)))]))

(define (is-relation? lst)
  (cond [(not (list? lst)) #f]
        [(empty? lst) #f]
        [else (and
               (not
                (member-fn
                 (lambda (item)
                  (cond [(not (list? item)) #t]
                        [(not (= (my-length item) 2)) #t]
                        [(member-fn
                           (lambda (inner-item) (not (integer? inner-item)))
                             item) #t]
                        [else #f]))
                 lst))
              (unique? lst))]))

; 8.
(define (is-function? lst)
  (and (is-relation? lst)
       (unique? (map (lambda (item) (first item)) lst))
       (unique? (map (lambda (item) (first (rest item))) lst))))

; Atom Counting

; 1.
(define (atom? x)
  (or (number? x) (symbol? x)))

; 2.
(define (count-atoms1 lst)
  (cond [(empty? lst) 0]
        [(atom? (first lst)) (+ 1 (count-atoms1 (rest lst)))]
        [else (count-atoms1 (rest lst))]))

; 3.
(define (count-atoms2 lst)
  (foldl (lambda (next accum) (if (atom? next) (+ accum 1) accum)) 0 lst))

; 4.
(define (check f suite)
  (let ([failed-tests 
         (foldr
          (lambda (next accum)
            (let* ([input (first next)]
                   [expected-output (first (rest next))]
                   [actual-output (f input)])
              (cond [(equal? actual-output expected-output) accum]
                    [else (cons (cons actual-output next) accum)])))
          '()
          suite)])
    (cond [(= (my-length failed-tests) 0) "all tests passed"]
          [else (map
                 (lambda (item)
                   (format "fail: (f ~s) returned ~s; expected ~s"
                           (first (rest item)) (first item) (my-last item)))
                 failed-tests)])))

; Testing

(define (count-atoms-bad1 lst) -1)

(define (count-atoms-bad2 lst) (my-length lst))

(define count-atoms-test-suite
  '((() 0)
    ((a) 1)
    ((2) 1)
    (("a") 0)
    ((a b) 2)
    ((3 1) 2)
    ((a 2) 2)
    (("a" b) 1)
    ((1 2 3 4) 4)
    ((a b c d) 4)
    ((1 a 2 3 b c) 6)
    ((1 a () 2 3 "m" b c (4 s 2)) 6)))

(define my-length-test-suite
  '((() 0)
    ((a) 1)
    ((a 1 (2 7)) 3)
    ((a 1 2 7) 4)))

(define onion-test-suite
  '((0 ())
    (1 (()))
    (2 ((())))
    (3 (((()))))
    (10 ((((((((((())))))))))))))

(define my-last-test-suite
  '(((a b c d) d)
    ((a b (c d)) (c d))))

(define double-up-test-suite
  '(((a 2 (c d)) (a a 2 2 (c d) (c d)))
    (() ())))

(define swap-ends-test-suite
  '((() ())
    ((a) (a))
    ((a b) (b a))
    ((a b c) (c b a))
    ((a b c d) (d b c a))))

(define softmax-sum-test-suite
  '(((-1 2 3) 1.0)
    ((8 7 -2) 1.0)
    ((-6) 1.0)))

(define is-relation-test-suite
  '((((2 4)) #t)
    (((6 2) (8 1) (2 2) (1 8)) #t)
    (((2 4) (1 5) (2 4)) #f)
    (((6.1 1) (1 5)) #f)
    (((6 1) (1 2 5)) #f)
    (() #f)
    ("a, b" #f)))

(define is-function-test-suite
  '((((2 4) (3 1) (1 8) (6 2)) #t)
    (((2 4) (1 5) (2 5)) #f)
    (((2 4) (3 1) (2 8)) #f)
    ('cat #f)))

(define (display-check-fn-result check-fn-output)
  (cond [(list? check-fn-output) (map (lambda (item) (printf "~s\n" item)) check-fn-output)]
        [else (printf "~s\n" check-fn-output)]))

(define (run-tests)
  (printf "\ntesting count-atoms-bad (expected failures)...\n")
  (display-check-fn-result (check count-atoms-bad1 count-atoms-test-suite))

  (printf "\ntesting count-atoms-bad2 (expected failures)...\n")
  (display-check-fn-result (check count-atoms-bad2 count-atoms-test-suite))
  
  (printf "\ntesting count-atoms1...\n")
  (display-check-fn-result (check count-atoms1 count-atoms-test-suite))

  (printf "\ntesting count-atoms2...\n")
  (display-check-fn-result (check count-atoms2 count-atoms-test-suite))

  (printf "\ntesting my-length...\n")
  (display-check-fn-result (check my-length my-length-test-suite))

  (printf "\ntesting onion...\n")
  (display-check-fn-result (check onion onion-test-suite))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "(onion -2) throws: ~s\n" (exn-message e)))])
    (onion -2))

  (printf "\ntesting my-last...\n")
  (display-check-fn-result (check my-last my-last-test-suite))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "(my-last '()) throws: ~s\n" (exn-message e)))])
    (my-last '()))

  (printf "\ntesting double-up...\n")
  (display-check-fn-result (check double-up double-up-test-suite))

  (printf "\ntesting swap-ends...\n")
  (display-check-fn-result (check swap-ends swap-ends-test-suite))

  (printf "\ntesting softmax...\n")
  (display-check-fn-result (check (lambda (input) (foldr + 0 (softmax input))) softmax-sum-test-suite))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "(softmax '()) throws: ~s\n" (exn-message e)))])
    (softmax '()))

  (printf "\ntesting is-relation?...\n")
  (display-check-fn-result (check is-relation? is-relation-test-suite))

  (printf "\ntesting is-function?...\n")
  (display-check-fn-result (check is-function? is-function-test-suite)))
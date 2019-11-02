#lang racket

; Helper function:
; Inclusively check if num is between start and end.
(define (in-range? num start end)
  (and (>= num start) (<= num end)))

; 1.
(define (is-digit? c)
  (cond [(char? c) (in-range? (char->integer c)
                              (char->integer #\0)
                              (char->integer #\9))]
        [else #f]))

; 2.
(define (is-letter? c)
  (cond [(char? c)
         (let ([code-point (char->integer c)])
           (or (in-range? code-point (char->integer #\a) (char->integer #\z))
               (in-range? code-point (char->integer #\A) (char->integer #\Z))))]
        [else #f]))

; 3.
(define (is-vble? x)
  (match x
    ['t #f]
    ['f #f]
    [id
     (and
      ; Must be a symbol
      (symbol? id)
      ; Convert symbol to string then to list of characters.
      (let ([char-list (string->list (symbol->string id))])
        (and
         ; First char must be letter.
         (is-letter? (first char-list))
         ; Remaining chars can be letter or digit.
         (andmap
          (lambda (char) (or (is-letter? char) (is-digit? char)))
          (rest char-list)))))]))

; Helper function:
; Check if x is valid bool literal ('t or 'f).
(define (is-bool-literal? x)
  (match x
    ['t #t]
    ['f #t]
    [_ #f]))

; 4.
(define (is-bool-env? x)
  (cond [(list? x)
         (andmap
          (lambda (item)
            (match item
              [`(,var ,truth-value)
               (and (is-vble? var)
                    (is-bool-literal? truth-value))] 
              [_ #f]))
          x)]
        [else #f]))

; Helper function:
; Apply actions to production rules specified in the EBNF.
;
; **Parameters**
; - expr: expression that should follow expr production rule.
;     expr =   variable | bool-literal
;            | not-expr | and-expr | or-expr | implication-expr .
; - not-action: action for not-expr produdction rule.
;     not-expr = "(" "not" expr ")" .
; - and-action: action for add-expr produdction rule.
;     and-expr = "(" expr "and" expr ")" .
; - or-action: action for or-expr produdction rule.
;     or-expr = "(" expr "or" expr ")" .
; - implication-action: action for implication-expr produdction rule.
;     implication-expr = "(" expr "-->" expr ")" .
; - vble-action: action for variable produdction rule.
;     variable = a symbol v for which (is-vble? v) returns #t .
; - bool-action: action for bool produdction rule.
;     bool-literal = "t" | "f" .
; - syntax-error-action: action when expr violates expr production rule.
(define
  (apply-grammar-actions expr
                         not-action
                         and-action
                         or-action
                         implication-action
                         vble-action
                         bool-action
                         syntax-error-action)
  (match expr
    [`(not ,expr1) (not-action expr1)]
    [`(,expr1 and ,expr2) (and-action expr1 expr2)]
    [`(,expr1 or ,expr2) (or-action expr1 expr2)]
    [`(,expr1 --> ,expr2) (implication-action expr1 expr2)]
    [(? is-vble? v) (vble-action v)]
    [(? is-bool-literal? b) (bool-action b)]
    [_ (syntax-error-action)]))

; 5.
(define (is-bool-expr? expr)
  (let ([binary-op-action
         (lambda (expr1 expr2)
           (and (is-bool-expr? expr1) (is-bool-expr? expr2)))])
    (apply-grammar-actions
     expr
     (lambda (expr1) (is-bool-expr? expr1))
     binary-op-action
     binary-op-action
     binary-op-action
     (lambda (v) #t)
     (lambda (b) #t)
     (lambda () #f))))

; Helper function:
; Convert racket primitive bool (#t or #f) to our custom bool-literal ('t or 'f).
(define (to-bool-literal x)
  (match x
    [#t 't]
    [#f 'f]
    [_ (error ". . to-bool-literal: expected (boolean? x) to be true")]))

; Helper function:
; Convert our custom bool-literal ('t or 'f) to racket bool (#t or #f).
(define (to-bool x)
  (match x
    ['t #t]
    ['f #f]
    [_ (error ". . to-bool: exptected (is-bool-literal? x) to be true")]))

; 6.
(define (eval-bool expr env)
  (cond
    [(not (is-bool-env? env))
     (error ". . eval-bool: expected (is-bool-env env) to be true")]
    [else
     (apply-grammar-actions
      expr

      ; not-action:
      (lambda (expr1)
        ; Convert operand to bool then call (not) on it
        ; then convert result back to bool-literal.
        (to-bool-literal (not (to-bool (eval-bool expr1 env)))))

      ; and-action:
      (lambda (expr1 expr2)
        ; Convert both operands to bool then call (or) on them
        ; then convert result back to bool-literal.
        (to-bool-literal
         (and
          (to-bool (eval-bool expr1 env))
          (to-bool (eval-bool expr2 env)))))

      ; or-action:
      (lambda (expr1 expr2)
        ; Convert both operands to bool then call (and) on them
        ; then convert result back to bool-literal.
        (to-bool-literal
         (or
          (to-bool (eval-bool expr1 env))
          (to-bool (eval-bool expr2 env)))))

      ; implication-action:
      (lambda (expr1 expr2)
        ; (p --> q) <=> ((not p) or q)
        (eval-bool `((not ,expr1) or ,expr2) env))

      ; vble-action:
      (lambda (v)
        (let ([truth-value (assoc v env)])
          (cond [truth-value (second truth-value)]
                [(error ". . eval-bool: undefined variable")])))

      ; bool-action:
      (lambda (b) b)

      ; syntax-error-action:
      (lambda () (error ". . eval-bool: syntax error")))]))

; Helper function:
; Return the union of lst1 and lst2.
; Both lst1 and lst2 must be unique (no duplicates).
(define (union lst1 lst2)
  (cond [(check-duplicates lst1) (error ". . union: lst1 has duplicates")]
        [(check-duplicates lst2) (error ". . union: lst2 has duplicates")]
        [else (foldl
               (lambda (next accum)
                 (cond [(member next accum) accum]
                       [else (cons next accum)]))
               lst1
               lst2)]))

; 7.
(define (get-vbles expr)
  (let ([binary-op-action
         (lambda (expr1 expr2)
           (union (get-vbles expr1) (get-vbles expr2)))])
    (apply-grammar-actions
     expr
     (lambda (expr1) (get-vbles expr1))
     binary-op-action
     binary-op-action
     binary-op-action
     (lambda (v) (list v))
     (lambda (b) (list))
     (lambda () (error ". . get-vbles: syntax error")))))

; Helper function:
; Convert num to list of 't and 'f symbols that represents num in binary.
; E.g. if num is 5 then output is '(t f t).
(define (to-binary-list num)
  (map (lambda (bit)
         (match bit
           [#\1 't]
           [#\0 'f]))
       (string->list (number->string num 2))))

; Helper function:
; Return new list with leading 'f symbols added so it's size is bit-size.
; Return binary-list as is if bit-size < binary-list length.
; E.g. (apply-padding '(t f t) 5) return '(f f t f t).
(define (apply-padding binary-list bit-size)
  (let ([padding-size (- bit-size (length binary-list))])
    (cond [(> padding-size 0)
           (append (build-list
                    (- bit-size (length binary-list))
                    (lambda (x) 'f))
                   binary-list)]
          [else binary-list])))

; 8.
(define (all-truth-values n)
  (cond [(< n 1) (error ". . all-truth-values: n < 1")]
        [else
         (build-list
          (expt 2 n)
          (lambda (num)
            (apply-padding (to-binary-list num) n)))]))

; 9.
(define (sat expr)
  (let* ([possible-envs
          (let ([vbles (get-vbles expr)])
            (cond [(empty? vbles)
                   ; No vbles mean only one possible env with no vbles 
                   '(())]
                  [else
                   ; Create list of envs with all possible truth values
                   ; for every variable in expr.
                   (map
                    ; "Merge" vbles and truth-value to form
                    ; a proper bool-env list.
                    (lambda (truth-value) (map list vbles truth-value))
                    (all-truth-values (length vbles)))]))]
         [sat-solns
          ; Find an env that makes (eval-bool expr env) return 't.
          (memf (lambda (env) (equal? (eval-bool expr env) 't))
                possible-envs)])
    (cond [sat-solns (first sat-solns)]
          [else 'unsat])))

; Unit test all functions asked in the assignment.
; Does not unit test helper functions.
(define (run-unit-tests)
  (local-require rackunit rackunit/text-ui)
  (run-tests
   (test-suite
    "all tests"
    (test-suite
     "is-digit? tests"
     (check-equal? (is-digit? #\0) #t)
     (check-equal? (is-digit? #\9) #t)
     (check-equal? (is-digit? #\4) #t)
     (check-equal? (is-digit? "4") #f)
     (check-equal? (is-digit? '(cheese shoes string)) #f)
     (check-equal? (andmap is-digit? (string->list "0123456789")) #t)
     (check-equal? (is-digit? #\/) #f)
     (check-equal? (is-digit? #\:) #f))

    (test-suite
     "is-letter? tests"
     (check-equal? (is-letter? #\A) #t)
     (check-equal? (is-letter? #\Z) #t)
     (check-equal? (is-letter? #\a) #t)
     (check-equal? (is-letter? #\z) #t)
     (check-equal? (is-letter? #\m) #t)
     (check-equal? (is-letter? #\Q) #t)
     (check-equal? (is-letter? 's) #f)
     (check-equal? (is-letter? "T") #f)
     (check-equal? (is-letter? '(cheese shoes string)) #f)
     (check-equal? (andmap is-letter? (string->list "OnceUponAtime")) #t)
     (check-equal? (is-letter? #\@) #f)
     (check-equal? (is-letter? #\[) #f)
     (check-equal? (is-letter? #\`) #f)
     (check-equal? (is-letter? #\{) #f))

    (test-suite
     "is-vble? tests"
     (check-equal? (is-vble? 'x) #t)
     (check-equal? (is-vble? 'flag) #t)
     (check-equal? (is-vble? '4tune) #f)
     (check-equal? (is-vble? 'c34) #t)
     (check-equal? (is-vble? 't) #f)
     (check-equal? (is-vble? 'f) #f)
     (check-equal? (is-vble? 'f3) #t)
     (check-equal? (is-vble? `big-val) #f)
     (check-equal? (is-vble? `BigVal) #t)
     (check-equal? (is-vble? "flag") #f)
     (check-equal? (is-vble? '(cheese shoes string)) #f))

    (test-suite
     "is-bool-env? tests"
     (check-equal? (is-bool-env? '((p t) (p f) (q f) (r f))) #t)
     (check-equal? (is-bool-env? '((p t) (q f) (r #f))) #f)
     (check-equal? (is-bool-env? '((p #t) (q f) (r f))) #f)
     (check-equal? (is-bool-env? '((p t) (4q f) (r f))) #f) 
     (check-equal? (is-bool-env? '((f t) (p f) (q f) (r f))) #f)
     (check-equal? (is-bool-env? '(cheese shoes string)) #f)
     (check-equal? (is-bool-env? '((p t) (q f c))) #f)
     (check-equal? (is-bool-env? '((p t) ())) #f)
     (check-equal? (is-bool-env? '()) #t))

    (test-suite
     "is-bool-expr? tests"
     (check-equal? (is-bool-expr? 't) #t)
     (check-equal? (is-bool-expr? '(t or (not f))) #t)
     (check-equal? (is-bool-expr? '((p and (p --> f)) --> q)) #t)
     (check-equal? (is-bool-expr? '(cheese shoes string)) #f))

    (test-suite
     "eval-bool tests"
     (check-equal? (eval-bool 'f '((a t) (b f) (c f) (d t) (a f))) 'f)
     (check-equal? (eval-bool '(t and t) '((a t) (b f) (c f) (d t) (a f))) 't)
     (check-equal? (eval-bool 'a '((a t) (b f) (c f) (d t) (a f))) 't)
     (check-equal?
      (eval-bool '(not (c --> d)) '((a t) (b f) (c f) (d t) (a f)))
      'f)
     (check-equal?
      (eval-bool '((c and a) --> (d or c)) '((a t) (b f) (c f) (d t) (a f)))
      't)
     (check-exn
      exn:fail?
      (lambda () (eval-bool 'x '((a t) (b f) (c f) (d t) (a f)))))
     (check-exn exn:fail? (lambda () (eval-bool '((p or q) --> p) '())))
     (check-exn exn:fail? (lambda () (eval-bool 't '((1 2 3) (4 5)))))
     (check-exn exn:fail? (lambda () (eval-bool '(hi there) '())))
     (check-exn exn:fail? (lambda () (eval-bool '() '((hiThere t))))))

    (test-suite
     "get-vbles tests"
     ; Num vbles > 1 cases.
     (let ([verify-expected-vbles
            (lambda (actual-vbles expected-vbles)
              (check-not-false
               (andmap
                (lambda (expected-vble)
                  (member expected-vble actual-vbles))
                expected-vbles)))])
       (verify-expected-vbles (get-vbles '((p or (not q)) --> (not p))) '(p q))
       (verify-expected-vbles (get-vbles '((d or a) and (c or b))) '(a b c d)))
     ; Num vbles <= 1 cases.
     (check-equal? (get-vbles '((f or (not t)) --> (not p))) '(p))
     (check-equal? (get-vbles 'r) '(r))
     (check-equal? (get-vbles 't) '())
     ; Syntax error cases
     (check-exn exn:fail? (lambda () (get-vbles '(hi there))))
     (check-exn exn:fail? (lambda () (get-vbles '()))))

    (test-suite
     "all-truth-values tests"
     ; n < 1 cases (should throw exception).
     (check-exn exn:fail? (lambda () (all-truth-values -1)))
     (check-exn exn:fail? (lambda () (all-truth-values 0)))
     ; n >=1 cases.
     (check-equal? (all-truth-values 1) '((f) (t)))
     (check-equal? (all-truth-values 2) '((f f) (f t) (t f) (t t)))
     (check-equal? (all-truth-values 3)
                   '((f f f)
                     (f f t)
                     (f t f)
                     (f t t)
                     (t f f)
                     (t f t)
                     (t t f)
                     (t t t)))
     (check-equal? (length (all-truth-values 8)) (expt 2 8)))

    (test-suite
     "sat tests"
     ; Satisfiable test cases.
     (let ([check-satisfiable
            (lambda (expr)
              (check-equal? (eval-bool expr (sat expr)) 't))])
       (check-satisfiable '((p or q) --> p))
       (check-satisfiable '(p --> p))
       (check-satisfiable '((not p) --> p))
       (check-satisfiable '(f --> t))
       (check-satisfiable '((f and t) or t))
       (check-satisfiable 't))
     ; Unsatisfiable test cases
     (check-equal? (sat '(p and (not p))) 'unsat)
     (check-equal? (sat '((p --> q) and (not (p --> q)))) 'unsat)
     (check-equal? (sat '(((not p) or q) and (not (p --> q)))) 'unsat)
     (check-equal? (sat '((t or f) and f)) 'unsat)
     (check-equal? (sat 'f) 'unsat)))))
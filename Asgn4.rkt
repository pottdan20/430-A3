#lang typed/racket
(require typed/rackunit)
;; Dane Potter
;; Berkeley Reynolds

;; Full project implemented

;; organization:  Definitions -> parser -> interpreter

;; Data definitions
(define-type ExprC (U NumC BinopC Leq0 IdC AppC))
(struct NumC ([n : Real]) #:transparent)
;;function structs
(struct FdC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent) ;; an ID element
(struct AppC ([fun : Symbol] [exprs : (Listof ExprC)]) #:transparent) ;; application of a func
; binop
(struct BinopC ([operator : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
; conditional
(struct Leq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

(define invalid-id-hash
  (hash '+ +
        '- -
        '* *
        '/ /
        'def 'def
        'leq0? 'leq0?
        'then 'then
        'else 'else
        '= '=))

;; takes any and returns if it is a valid-id as a bool 
(define (valid-id? [id : Any]) : Boolean
  (match id
    [(? symbol? s) (not (hash-has-key? invalid-id-hash s))]
    [other false]))
;; validates a list of ids
(define (valid-ids? [ids : (Listof Any)]) : Boolean
  (match ids
    ['() true]
    [(cons f r) (and (valid-id? f) (valid-ids? r))]))


(check-equal? (valid-id? 'hi) true)
(check-equal? (valid-id? "hi") false)
(check-equal? (valid-id? '/) false)
(check-equal? (valid-id? 'leq0) true)
(check-equal? (valid-id? 'leq0?) false)
(check-equal? (valid-id? '=) false)
(check-equal? (valid-id? '==) true)
(check-equal? (valid-id? 10) false)

;; cast-args-to-symbols
(define (cast-args-to-symbols [args : (Listof Any)]) : (Listof Symbol)
  (match args
    [(cons f r) (cons (cast f Symbol) (cast-args-to-symbols r))]
    ['() '()]))

;; placeholder fds (function definitions) that will be used for testing
(define testFds ( list (FdC 'f (list 'x) (BinopC '+ (NumC 2) (IdC 'x))) (FdC 'g (list 'y) (BinopC '+ (NumC 5) (IdC 'y)))))


;; given a symbol and fds, returns the FdC with the given name, if possible.
;; else it will thow an error
(define (get-fundef [sym : Symbol] [fundefs : (Listof FdC)]) : FdC
  (match fundefs
    ['() (error "VVQS: No function found with the name: ~e" sym)]
    [(cons (FdC s arg b) r)
     (cond
       [(symbol=? s sym) (first fundefs)]
       [else (get-fundef sym r)])]))

#;[(check-equal? (get-fundef 'main (list  (FdC 'main 'init (AppC 'double (NumC 7)))))
              (FdC 'main 'init (AppC 'double (NumC 7))))
(check-equal? (get-fundef 'f1 (list  (FdC 'main 'init (AppC 'double (NumC 7)))
                                     (FdC 'f1 'x (BinopC '+ (NumC 1) (IdC 'x)))))
              (FdC 'f1 'x (BinopC '+ (NumC 1) (IdC 'x))))
(check-exn #rx"VVQS"
           (lambda() (get-fundef 'f2 (list  (FdC 'main 'init (AppC 'double (NumC 7)))
                                            (FdC 'f1 'x (BinopC '+ (NumC 1) (IdC 'x)))))))
(check-exn #rx"VVQS"
           (lambda() (get-fundef 'NoFunc (list (FdC 'main 'init (AppC 'double (NumC 7)))))))

]

;; get-operator takes a symbol and returns its actual operator, if possible
;; otherwise, an error is thrown
(define (get-operator [operator : Symbol]) : (-> Real Real Real)
  (match operator
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]
    [other (error "VVQS: error -- expected a valid operator (+, -, *, /), got ~e" other)]))

(check-equal? (get-operator '+) +)
(check-equal? (get-operator '-) -)
(check-equal? (get-operator '*) *)
(check-equal? (get-operator '/) /)
(check-exn #rx"VVQS" (lambda() (get-operator '\))))
(check-exn #rx"VVQS" (lambda() (get-operator 'a)))
(check-exn #rx"VVQS" (lambda() (get-operator 'asdf)))




;; PARSER

;; takes an Sexp and returns the ExprC corresponding to the Sexp, if applicable.
;; otherwise, an error is thrown
(define (parse [expr : Sexp]) : ExprC
 (match expr
     [(? real? n) (NumC n)]
     #;[(list (? symbol? s) l r) (cond
                                 [(not (valid-id? s)) (BinopC s (parse l) (parse r))]
                                 [else (error "VVQS: error -- expected valid operator, got ~e" s)])]
     [(list 'leq0? test 'then then 'else else) (Leq0 (parse test) (parse then) (parse else))]
     [(? symbol? sym) (cond
                        [(valid-id? sym) (IdC (cast sym Symbol))]
                        [else (error "VVQS: error -- expected valid id, got ~e" sym)])]
     [(list (? symbol? s) args ...) (cond
                                 [(valid-id? s) (AppC s (parse-args args))]
                                 [(and (= (length args) 2) (not (valid-id? s))) (BinopC s (parse (first args)) (parse (second args)))]
                                 [else (error "VVQS: error -- expected valid id and got: ~e" args)]
                                )]
     [other (error "VVQS: error -- expected expression, got ~e" other)]))

;;parse-args parses a list of Sexp and returns a list of ExprC
(define (parse-args [args : (Listof Sexp)]) : (Listof ExprC)
  (match args
    ['() '()]
    [(cons f r) (cons (parse f) (parse-args r))])) 

(check-equal? (parse '{call 3 4}) (AppC 'call (list (NumC 3) (NumC 4))))
(check-equal? (parse '{call}) (AppC 'call '()))
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse '{+ 2 3}) (BinopC '+ (NumC 2) (NumC 3)))
(check-equal? (parse '{* 2 3}) (BinopC '* (NumC 2) (NumC 3)))
(check-equal? (parse '{* {+ 1 2} {+ 3 4}}) (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (BinopC '+ (NumC 3) (NumC 4))))
(check-equal? (parse '{+ {* 1 2} {* 3 4}}) (BinopC '+ (BinopC '* (NumC 1) (NumC 2)) (BinopC '* (NumC 3) (NumC 4))))

;(check-exn #rx"VVQS" (lambda() (parse '{a b c})))
(check-exn #rx"VVQS" (lambda() (parse '{+ 4})))
(check-exn #rx"VVQS" (lambda() (parse '{+ / 4})))
(check-exn #rx"VVQS" (lambda() (parse '{+})))
(check-exn #rx"VVQS" (lambda() (parse '{})))
(check-exn #rx"VVQS" (lambda() (parse '{+ {+ 1 2}})))
(check-exn #rx"VVQS" (lambda() (parse '{* {+ 1 2}})))
(check-exn #rx"VVQS" (lambda() (parse '{* {+ 1 2} {+ 1}})))

(check-equal? (parse '{leq0? 1
                             then 1
                             else {- 1 1}})
              (Leq0 (NumC 1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}))
(check-equal? (parse '{leq0? {+ 1 -2}
                             then {- 10 {+ 1 2}}
                             else {- 1 1}})
              (Leq0 (BinopC '+ (NumC 1) (NumC -2))
                    (BinopC '- (NumC 10) (BinopC '+ (NumC 1) (NumC 2)))
                    {BinopC '- (NumC 1) (NumC 1)}))


;; PARSE FUNDEFS
;; takes an Sexp and returns the FdC corresponding to the Sexp, if possible.
;; Otherwise, throws an error
(define (parse-fundef [expr : Sexp]) : FdC
  (match expr
    [(list 'def (list name args ...) '= body) (cond
                                                [(and (valid-id? name) (valid-ids? args))
                                                 (FdC (cast name Symbol) (cast-args-to-symbols args) (parse body))]
                                                [else
                                                 (error "VVQS: error -- expected valid function definition id, got ~e"
                                                        name)])]
    [other (error "VVQS: error -- expected valid function definition, got ~e" other)]))

(check-equal? (parse-fundef '{def {double x y} = {* x 2}}) (FdC 'double (list 'x 'y) (BinopC '* (IdC 'x) (NumC 2))))
(check-equal? (parse-fundef '{def {add-one x} = {+ x 1}}) (FdC 'add-one (list 'x) (BinopC '+ (IdC 'x) (NumC 1))))
(check-equal? (parse-fundef '{def {five} = 5}) (FdC 'five '() (NumC 5)))
(check-exn #rx"VVQS" (lambda() (parse-fundef '{def {/ x} = {* x 2}})))
(check-exn #rx"VVQS" (lambda() (parse-fundef '{def {double x} = })))
(check-exn #rx"VVQS" (lambda() (parse-fundef '{{double x} = {* x 2}})))
(check-exn #rx"VVQS" (lambda() (parse-fundef '{def = {* x 2}})))
(check-exn #rx"VVQS" (lambda() (parse-fundef '{def {+ x} = {* x 2}}))) 

;;PARSE-PROG
;; takes an Sexp and returns all function definitions in the Sexp as a list of FdCs, if possible.
;; otherwise, throws an error
(define (parse-prog [s : Sexp]) : (Listof FdC)
  (match s
    ['() '()]
    #;[(cons (list 'def (list name args ...) '= body) r) (cons (parse-fundef
                                                         (list 'def (list name args) '= body)) (parse-prog r))]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    #;[other (error "VVQS: code unbound by a function. got: ~e" other)])) ;--- commented out these lines to make multiple args smoother

(check-equal? (parse-prog '{{def {double x} = {* x 2}}
                            {def {tri f x z} = {* f {+ z x}}}
                            {def {main init} = {double 7}}})
              (list (FdC 'double (list 'x) (BinopC '* (IdC 'x) (NumC 2)))
                    (FdC 'tri (list 'f 'x 'z) (BinopC '* (IdC 'f) (BinopC '+ (IdC 'z) (IdC 'x))))
                    (FdC 'main (list 'init) (AppC 'double (list (NumC 7))))))
(check-equal? (parse-prog '{{def {double x} = {* x 2}}
                            {def {main} = {+ 5 2}}})
              (list (FdC 'double (list 'x) (BinopC '* (IdC 'x) (NumC 2)))
                    (FdC 'main '() (BinopC '+ (NumC 5) (NumC 2))) ))

(check-exn #rx"VVQS" (lambda() (parse-prog '{"asd"})))




;; INTERPETER
;; takes an ExprC and returns the result of the expression, if possible.
;; if not, an error is thrown
(define (interp [expr : ExprC] [fds : (Listof FdC)]): Real
  (match expr
    [(NumC n) n]
    [(BinopC op l r) (cond
                       [(and (equal? op '/) (equal? (interp r fds) 0)) (error "VVQS: Can not divide by 0")]
                       [else ((get-operator op) (interp l fds) (interp r fds))])]
    [(Leq0 test then else) (cond
                             [(<= (interp test fds) 0) (interp then fds)]
                             [else (interp else fds)]
                             )]
    [(AppC f args) (local ([define fd (get-fundef f fds)]) ;; from text book 
                  (interp (subst-all (interp-list args fds) 
                                 (FdC-args fd)
                                 (FdC-body fd)
                                 (make-tuple (FdC-args fd) (interp-list args fds)))
                          fds))]
    [(IdC x) (error "VVQS : interp shouldnt get here... unbound var")]))

;;interp-list takes list of ExprC and returns list of reals
(define (interp-list [args : (Listof ExprC)] [fds : (Listof FdC)]) : (Listof Real)
  (match args
  ['() '()]
  [(cons f r) (cons (interp f fds) (interp-list r fds))]))


;;interp tests below subst function
;; make tuple combines the 2 lists into a list of tuples
(define (make-tuple [for : (Listof Symbol)] [what : (Listof Real)]) : (Listof (List Symbol Real))
  (match for
    [(cons f r) (match what
                  [(cons f2 r2) (cons (list f f2) (make-tuple r r2))]
                  ['() (error "VVQS : invalid variable count")])]
    ['() (match what
         ['() '()]
         [(cons f r) (error "VVQS : invalid variable count")])]))

;;subst-all goes through the argument list and maps the correct vars to the correct values
(define (subst-all [what : (Listof Real)] [for : (Listof Symbol)] [in : ExprC] [pairs : (Listof (List Symbol Real))]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (NumC (get-real-from-sym pairs s))]
    [(AppC f args) (AppC f (map (lambda ([a : ExprC])
                                  (subst-all what for a pairs)) args))] 
    [(BinopC s l r) (BinopC s (subst-all what for l pairs) (subst-all what for r pairs))]
    [(Leq0 test then else) (Leq0 (subst-all what for test pairs) (subst-all what for then pairs) (subst-all what for else pairs))]))

;; get-real-from-sym takes tuple list and returns paired real from desired symbol
(define (get-real-from-sym [pairs : (Listof (List Symbol Real))] [sym : Symbol]) : Real
  (match pairs
    [(cons (list s real) r) (cond
                              [(symbol=? s sym) real]
                              [else (get-real-from-sym r sym)])]
    ['() (error "error line 285")]))


#;(check-equal? (subst-all (list 3 6) (list 'x 'y) (BinopC '+ (NumC 5) (IdC 'd))) (BinopC '+ (NumC 5) (IdC 'd)))
#;(check-equal? (subst-all 1 'x (Leq0 (BinopC '+ (NumC 1) (NumC 2)) (NumC 5) (NumC 10)))
              (Leq0 (BinopC '+ (NumC 1) (NumC 2)) (NumC 5) (NumC 10)))



;;interp tests
(check-equal? (interp (NumC 1) testFds) 1)
(check-equal? (interp (NumC 0) testFds) 0)
(check-equal? (interp (BinopC '+ (NumC 1) (NumC 2)) testFds) 3)
(check-equal? (interp (BinopC '* (NumC 1) (NumC 2)) testFds) 2)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 10)) (NumC 2)) testFds) 22)

(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 10)) (BinopC '+ (NumC 1) (NumC 2))) testFds) 33)
(check-equal? (interp (BinopC '+ (BinopC '+ (NumC 1) (NumC 10)) (BinopC '+ (NumC 1) (NumC 2))) testFds) 14)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 0) (NumC 0)) (BinopC '+ (NumC 0) (NumC 0))) testFds) 0)
(check-equal? (interp (BinopC '- (BinopC '+ (NumC 1) (NumC 2)) (BinopC '+ (NumC 1) (NumC 1))) testFds) 1)
(check-equal? (interp (BinopC '/ (BinopC '+ (NumC 2) (NumC 2)) (BinopC '+ (NumC 1) (NumC 1))) testFds) 2)
(check-equal? (interp (BinopC '/ (BinopC '* (NumC 2) (NumC 20)) (BinopC '- (NumC 7) (NumC 3))) testFds) 10)

(check-exn #rx"VVQS" (lambda() (interp (BinopC 'hi (NumC 1) (NumC 2)) testFds)))
(check-exn #rx"VVQS" (lambda() (interp (BinopC 'a (NumC 1) (BinopC '+ (NumC 1) (NumC 2))) testFds)))
(check-exn #rx"VVQS" (lambda() (interp (BinopC '+ (NumC 1) (BinopC 'a (NumC 1) (NumC 2))) testFds)))
(check-exn #rx"VVQS" (lambda() (interp (IdC 'v) testFds)))

(check-equal? (interp (Leq0 (NumC 1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}) testFds) 0)
(check-equal? (interp (Leq0 (NumC -1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}) testFds) 1)
(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC 2)) (NumC 1) {BinopC '- (NumC 10) (NumC 1)}) testFds) 9)
(check-equal? (interp
               (Leq0 (BinopC '+ (NumC 1) (NumC -2))
                     (BinopC '* (NumC 10) (NumC 10))
                     {BinopC '- (NumC 10) (NumC 1)}) testFds)
              100)
(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC 2))
                            (BinopC '* (NumC 10) (NumC 10))
                            {BinopC '- (NumC 10) (BinopC '* (NumC 1) (NumC 4))}) testFds) 6)
(check-equal? (interp (parse '(* 3 (+ 2 7))) testFds) 27)
(check-exn #rx"VVQS" (lambda() (interp (BinopC '/ (NumC 1) (NumC 0)) testFds)))


;; tests on both parse and interp
(check-equal? (interp (parse '1) testFds) 1)
(check-equal? (interp (parse '{+ 2 3}) testFds) 5)
(check-equal? (interp (parse '{* 2 3}) testFds) 6)
(check-equal? (interp (parse '{* {+ 1 2} {+ 3 4}}) testFds) 21)
(check-equal? (interp (parse '{+ {* 1 2} {* 3 4}}) testFds) 14)
(check-equal? (interp (parse '{+ {* 1 {* 4 2}} {* 3 {+ 6 4}}}) testFds) 38)
(check-equal? (interp (parse '{- {* 3 {* 4 2}} {* 2 {+ 6 4}}}) testFds) 4)
(check-equal? (interp (parse '{- {/ {* 4 6} 3} {* 2 {+ 1 2}}}) testFds) 2)
(check-equal? (interp (parse '{/ {* 10 {* 5 2}} {* 2 {- 7 2}}}) testFds) 10)
(check-equal? (interp (parse '{leq0? (+ 1 4)
                                     then (* 4 5)
                                     else (- 4 (+ 1 2))}) testFds) 1)
(check-equal? (interp (parse '{leq0? (+ 1 -4)
                                     then (* 4 5)
                                     else (- 4 (+ 1 2))}) testFds) 20)



;; Interpret-fns takes a list of FdCs and returns the interpretation of the main function
(define (interp-fns [funs : (Listof FdC)]) : Real
  (interp (AppC 'main (list (NumC 0))) funs))

(check-equal?  (interp-fns (parse-prog '{{def {double x} = {* 2 x}}
                            {def {main init} = {double 13}}})) 26)
(check-equal?  (interp-fns (parse-prog '{{def {add-one x} = {+ 1 x}}
                            {def {main init} = {add-one 13}}})) 14)

;; top-interp takes in Sexp in VVQS language and returns the interpreted output of a real number
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


(check-equal?  (top-interp '{{def {double x y} = {* 2 y}}
                            {def {main init} = {double 13 15}}}) 30)

(check-equal?  (top-interp '{{def {double x} = {* 2 x}}
                            {def {main init} = {double 13}}}) 26)

(check-equal?  (top-interp '{{def {add3together a b c} = {+ a {+ b c}}} 
                             {def {makeNeg x} = {* -1 x}}
                             {def {add5ThenMult2 x} = {* 2 {+ 5 x}}}
                             {def {main init} = {+ {+ {add2ThenNeg 12} {add5ThenMult2 2}} {add3together 2 5 9}}}
                             {def {add2ThenNeg x} = {makeNeg {+ 2 x}}}}) 16) 





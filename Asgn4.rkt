#lang typed/racket
(require typed/rackunit)
;; Dane Potter
;; Berkeley Reynolds

;; Full project implemented

;; organization:  Definitions -> parser -> interpreter

;; Data definitions
(define-type ExprC (U NumC BinopC Leq0 IdC AppC LamC StringC IfC))
(struct NumC ([n : Real]) #:transparent)
(struct StringC ([s : String])#:transparent)
(struct IfC ([do : ExprC] [if : ExprC] [else : ExprC])#:transparent)
;;function structs
(struct FdC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent) ;;lambda definition
(define-type Environment (Listof Binding))
;(struct Env ([env : (Listof Binding)]) #:transparent) 
(struct IdC ([s : Symbol]) #:transparent) ;; an ID element
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent) ;; application of a func
; binop
(struct BinopC ([operator : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
; conditional
(struct Leq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
;;
(define-type Value (U Real Boolean String CloV PrimV FunV))
(struct FunV ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)
(struct Binding ([s : Symbol] [v : Value]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)



(define invalid-id-hash
  (hash 'where 'where
        'if 'if
        'else 'else
        '=> '=>
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


;; cast-args-to-symbols
(define (cast-args-to-symbols [args : (Listof Any)]) : (Listof Symbol)
  (match args
    [(cons f r) (cons (cast f Symbol) (cast-args-to-symbols r))]
    ['() '()]))

;; placeholder fds (function definitions) that will be used for testing
(define testFds ( list (FdC 'f (list 'x) (BinopC '+ (NumC 2) (IdC 'x)))
                       (FdC 'g (list 'y) (BinopC '+ (NumC 5) (IdC 'y)))))


;; given a symbol and fds, returns the FdC with the given name, if possible.
;; else it will thow an error
(define (get-fundef [sym : Symbol] [fundefs : (Listof FdC)]) : FdC
  (match fundefs
    ['() (error "VVQS: No function found with the name: ~e" sym)]
    [(cons (FdC s arg b) r)
     (cond
       [(symbol=? s sym) (first fundefs)]
       [else (get-fundef sym r)])])) 



#;(check-exn #rx"VVQS"
           (lambda() (get-fundef 'NoFunc '())))

;; get-operator takes a symbol and returns its actual operator, if possible
;; otherwise, an error is thrown
(define (get-operator [operator : Symbol]) : (-> Real Real Value)
  (match operator
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]
    ['<= <=]
    ['equal? equal?]
    [other (error "VVQS: error -- expected a valid operator (+, -, *, /), got ~e" other)]))

#;(check-equal? (get-operator '+) +)
#;(check-equal? (get-operator '-) -)
#;(check-equal? (get-operator '*) *)
#;(check-equal? (get-operator '/) /)
#;(check-exn #rx"VVQS" (lambda() (get-operator '\))))
#;(check-exn #rx"VVQS" (lambda() (get-operator 'a)))
#;(check-exn #rx"VVQS" (lambda() (get-operator 'asdf)))


;; serialize takes any VVQS5 value, and returns a string
(define (serialize [val : Any]) : String
  (match val
    [(? real? val) (~v val)]
    [#t "true"]
    [#f "false"]
    [(? string? val) (~v val)]
    [(? CloV? val) "#<procedure>"]
    [(? PrimV? val) "#<primop>"]
    [other (error "VVQS: Expected valid VVQS5 value, got: ~e" val)]))

(check-equal? (serialize 14) "14")
(check-equal? (serialize #t) "true")
(check-equal? (serialize #f) "false")
(check-equal? (serialize true) "true")
(check-equal? (serialize false) "false")
(check-equal? (serialize "hi") "\"hi\"")
(check-equal? (serialize "") "\"\"")
(check-equal? (serialize (CloV (list '+ '-)
                               (NumC 1)
                               (list (Binding '+ 10) (Binding '- 20))))
                         "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")




;; PARSER 

;; takes an Sexp and returns the ExprC corresponding to the Sexp, if applicable.
;; otherwise, an error is thrown
(define (parse [expr : Sexp]) : ExprC
  (match expr
   [(? real? n) (NumC n)]
   [(? string? s) (StringC s)]
   [(list expr1 'if expr2 'else expr3) (IfC (parse expr1) (parse expr2) (parse expr3))]
   ;[(list expr1 'where (list a ':= b) ...) (AppC (LamC (cast a (Listof Symbol)) (parse expr1)) (cast b (Listof ExprC)))]
   [(list expr1 'where (list (list a ':= b) ...)) (AppC (LamC (cast a (Listof Symbol)) (parse expr1)) (cast (map (lambda (x) (parse x)) (cast b (Listof Sexp))) (Listof ExprC)) )]
   [(list (list (? symbol? syms) ...) '=> expr) (LamC (cast syms (Listof Symbol)) (parse expr))]
   [(? symbol? sym) (cond
                      [(valid-id? sym) (IdC (cast sym Symbol))]
                      [else (error "VVQS: error -- expected valid id, got ~e" sym)])]
   [(list lam exprs ...) (AppC (parse lam) (map (lambda (x) (parse x)) exprs))]
   [other (error "VVQS: error -- expected expression, got ~e" other)]))




 
;;parse-args parses a list of Sexp and returns a list of ExprC
#;(define (parse-args [args : (Listof Sexp)]) : (Listof ExprC)
  (match args
    ['() '()]
    [(cons f r) (cons (parse f) (parse-args r))])) 

;(check-equal? (parse '{call 3 4}) (AppC (IdC 'call) (list (NumC 3) (NumC 4))))
(check-equal? (parse '{{x} => 5}) (LamC (list 'x) (NumC 5)))
(check-equal? (parse '{{+ x y} where {[x := 5] [y := 7]}}) (AppC (LamC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 5) (NumC 7))))
#;(check-equal? (parse '1) (NumC 1))
#;(check-equal? (parse '{+ 2 3}) (BinopC '+ (NumC 2) (NumC 3)))
#;(check-equal? (parse '{* 2 3}) (BinopC '* (NumC 2) (NumC 3)))
#;(check-equal? (parse '{* {+ 1 2} {+ 3 4}}) (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (BinopC '+ (NumC 3) (NumC 4))))
#;(check-equal? (parse '{+ {* 1 2} {* 3 4}}) (BinopC '+ (BinopC '* (NumC 1) (NumC 2)) (BinopC '* (NumC 3) (NumC 4))))


#;(check-exn #rx"VVQS" (lambda() (parse '{+ 4})))
#;(check-exn #rx"VVQS" (lambda() (parse '{+ / 4})))
#;(check-exn #rx"VVQS" (lambda() (parse '{+})))
#;(check-exn #rx"VVQS" (lambda() (parse '{})))
#;(check-exn #rx"VVQS" (lambda() (parse '{+ {+ 1 2}})))
#;(check-exn #rx"VVQS" (lambda() (parse '{* {+ 1 2}})))
#;(check-exn #rx"VVQS" (lambda() (parse '{* {+ 1 2} {+ 1}})))

#;(check-equal? (parse '{leq0? 1
                             then 1
                             else {- 1 1}})
              (Leq0 (NumC 1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}))
#;(check-equal? (parse '{leq0? {+ 1 -2}
                             then {- 10 {+ 1 2}}
                             else {- 1 1}})
              (Leq0 (BinopC '+ (NumC 1) (NumC -2))
                    (BinopC '- (NumC 10) (BinopC '+ (NumC 1) (NumC 2)))
                    {BinopC '- (NumC 1) (NumC 1)}))


;; PARSE FUNDEFS
;; takes an Sexp and returns the FdC corresponding to the Sexp, if possible.
;; Otherwise, throws an error
#;(define (parse-fundef [expr : Sexp]) : FdC
  (match expr
    [(list 'def (list name args ...) '= body) (cond
                                                [(and (valid-id? name) (valid-ids? args))
                                                 (FdC (cast name Symbol) (cast-args-to-symbols args) (parse body))]
                                                [else
                                                 (error "VVQS: error -- expected valid function definition id, got ~e"
                                                        name)])]
    [other (error "VVQS: error -- expected valid function definition, got ~e" other)]))

;;PARSE-PROG
;; takes an Sexp and returns all function definitions in the Sexp as a list of FdCs, if possible.
;; otherwise, throws an error
#;(define (parse-prog [s : Sexp]) : (Listof FdC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))
     




;; INTERPETER
;; takes an ExprC and returns the result of the expression, if possible.
;; if not, an error is thrown
(define (interp [expr : ExprC] [env : Environment]): Value
  (match expr
    [(NumC n) n]
    [(StringC s) s]
    [(FdC name args body) (FunV name args body)]  
    [(LamC args body) (CloV args body env)]
    [(IfC do if else) (local ([define condition (interp if env)])
                        (match condition
                          [#t (interp do env)]
                          [#f (interp else env)]
                          [else ((error "VVQS: error -- expected boolean expression in if, got ~e" if))]))]
    [(AppC f args) (local ([define f-value (interp f env)])
                                              ;; from text book
                     (match f-value
                       [(CloV as bod CloEnv) (interp bod
                                              (extend-env (bind as
                                                                (map (lambda ([a : ExprC]) (interp a env)) args))
                                                          CloEnv))] ;; interp each arg with current env and then add to closure env?
                       [(PrimV s) (match args
                                    [(cons l (cons r '())) (local ([define real-l (interp l env)]
                                                                   [define real-r (interp r env)])
                                                             (match real-l
                                                               [(? real? rl) (match real-r
                                                                               [(? real? rr) ((get-operator s) rl rr)]
                                                                               [other (error "VVQS: error -- binary operator must take two reals, got ~e" args)])]
                                                               [other (error "VVQS: error -- binary operator must take two reals, got ~e" args)]
                                                             ))]
                                    [other (error "VVQS: error -- expected two arguments, got ~e" args)])]
                       [other f-value]) 
                      
                     )]
                     ;; check what value V is
                     ;;primV - pass to a primV func that takes args and primV
                     ;;cloV - replace IdCs with env lookups
                     ;;
                     
                       ;[(primV o) ...]
                       ;[(cloV args body enviro) ... nested?]
                       ;[(NumC n)])
                     #;(interp (subst-all (interp-list body) env)
                          env)
    [(IdC x) (env-lookup x env)]))



;;extend env extends env
(define (extend-env [e1 : Environment] [e2 : Environment]) : Environment ;; adds new first because lookup goes from front to back
  (match e2
    ['() e1]
    [(cons f r) (cons f (extend-env r e1))])
  )

;;env-lookup loops through the env and returns the ExprC matched to a symbol binding
(define (env-lookup [sym : Symbol] [env : Environment]) : Value
   (match env
    [(cons (Binding s v) r) (cond
                              [(symbol=? s sym) v]
                              [else (env-lookup sym r)])]
    ['() (error "VVQS: no var in environment ~e" sym)]))

  

;;interp-list takes list of ExprC and returns list of reals
#;(define (interp-list [args : (Listof ExprC)] [env : Environment]) : (Listof Real)
  (match args
  ['() '()]
  [(cons f r) (cons (interp f env) (interp-list r env))]))


;;interp tests below subst function
;; make tuple combines the 2 lists into a list of tuples
(define (bind [for : (Listof Symbol)] [what : (Listof Value)]) : (Listof Binding)
  (match for 
    [(cons f r) (match what
                  [(cons f2 r2) (cons (Binding f f2) (bind r r2))]
                  ['() (error "VVQS : invalid variable count")])]
    ['() (match what
         ['() '()]
         [(cons f r) (error "VVQS : invalid variable count")])]))

#;(check-equal? (make-tuple '() '()) '())
#;(check-equal? (make-tuple '(+) '(1)) '((+ 1)))
#;(check-equal? (make-tuple '(+ -) '(1 2)) '((+ 1) (- 2)))
#;(check-exn #rx"VVQS"
           (lambda() (make-tuple '(+ -) '(1))))
#;(check-exn #rx"VVQS"
           (lambda() (make-tuple '(+) '(1 2))))


;;subst-all goes through the argument list and maps the correct vars to the correct values 
#;(define (subst-all [what : (Listof Value)]
                   [for : (Listof Symbol)]
                   [in : ExprC]
                   [pairs : (Listof (List Symbol Real))]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (NumC (get-real-from-sym pairs s))]
    [(AppC f args) (AppC f (map (lambda ([a : ExprC])
                                  (subst-all what for a pairs)) args))] 
    [(BinopC s l r) (BinopC s (subst-all what for l pairs) (subst-all what for r pairs))]
    [(Leq0 test then else) (Leq0 (subst-all what for test pairs)
                                 (subst-all what for then pairs)
                                 (subst-all what for else pairs))]))




;; get-real-from-sym takes tuple list and returns paired real from desired symbol
(define (get-real-from-sym [pairs : (Listof (List Symbol Real))] [sym : Symbol]) : Real
  (match pairs
    [(cons (list s real) r) (cond
                              [(symbol=? s sym) real]
                              [else (get-real-from-sym r sym)])]
    ['() (error "VVQS: error line 285")]))  



;; tests for subst-all and get-real-from-sym
#;(check-equal? (subst-all '(1) '(x) (Leq0 (NumC 1) (NumC 10) (IdC 'x)) '((x -10))) (Leq0 (NumC 1) (NumC 10) (NumC -10)))
#;(check-exn #rx"VVQS"
           (lambda() (get-real-from-sym '() 'x)))


;;interp tests 

(define testEnv1 
  (bind (list 'f 'g 'x) (list
                         (CloV (list 'y 'x) (AppC (IdC 'y) (list (IdC 'x))) '())
                         9
                         3)))
(check-equal? (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (NumC 6))) testEnv1) 6)


(define testEnv2 
  (bind (list 'f 'true 'false) (list (CloV (list 'y 'x) (AppC (IdC 'y) (list (IdC 'x))) '()) #t #f)))

(check-equal? (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (IdC 'true))) testEnv2) #t)
(check-equal? (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (IdC 'false))) testEnv2) #f)

(check-equal? (serialize (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (IdC 'false))) testEnv2)) "false")


;(check-equal? (interp (AppC (IdC 'f) (list (NumC 3) (IdC 'x))) testEnv1) 3)

;(check-equal? (interp (LamC (list 'a 'b 'c) (LamC (list 'l) (IdC 'b))) testEnv1) 4)
;(check-equal? (interp (LamC (list 'a 'b 'c) (IdC 'b)) testEnv1) 5)

#;(check-equal? (interp (NumC 1) testFds) 1)
#;(check-equal? (interp (NumC 0) testFds) 0)
#;(check-equal? (interp (BinopC '+ (NumC 1) (NumC 2)) testFds) 3)
#;(check-equal? (interp (BinopC '* (NumC 1) (NumC 2)) testFds) 2)
#;(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 10)) (NumC 2)) testFds) 22)

#;(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 10)) (BinopC '+ (NumC 1) (NumC 2))) testFds) 33)
#;(check-equal? (interp (BinopC '+ (BinopC '+ (NumC 1) (NumC 10)) (BinopC '+ (NumC 1) (NumC 2))) testFds) 14)
#;(check-equal? (interp (BinopC '* (BinopC '+ (NumC 0) (NumC 0)) (BinopC '+ (NumC 0) (NumC 0))) testFds) 0)
#;(check-equal? (interp (BinopC '- (BinopC '+ (NumC 1) (NumC 2)) (BinopC '+ (NumC 1) (NumC 1))) testFds) 1)
#;(check-equal? (interp (BinopC '/ (BinopC '+ (NumC 2) (NumC 2)) (BinopC '+ (NumC 1) (NumC 1))) testFds) 2)
#;(check-equal? (interp (BinopC '/ (BinopC '* (NumC 2) (NumC 20)) (BinopC '- (NumC 7) (NumC 3))) testFds) 10)

#;(check-exn #rx"VVQS" (lambda() (interp (BinopC 'hi (NumC 1) (NumC 2)) testFds)))
#;(check-exn #rx"VVQS" (lambda() (interp (BinopC 'a (NumC 1) (BinopC '+ (NumC 1) (NumC 2))) testFds)))
#;(check-exn #rx"VVQS" (lambda() (interp (BinopC '+ (NumC 1) (BinopC 'a (NumC 1) (NumC 2))) testFds)))
#;(check-exn #rx"VVQS" (lambda() (interp (IdC 'v) testFds)))

#;(check-equal? (interp (Leq0 (NumC 1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}) testFds) 0)
#;(check-equal? (interp (Leq0 (NumC -1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}) testFds) 1)
#;(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC 2)) (NumC 1) {BinopC '- (NumC 10) (NumC 1)}) testFds) 9)
#;(check-equal? (interp
               (Leq0 (BinopC '+ (NumC 1) (NumC -2))
                     (BinopC '* (NumC 10) (NumC 10))
                     {BinopC '- (NumC 10) (NumC 1)}) testFds)
              100)
#;(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC 2))
                            (BinopC '* (NumC 10) (NumC 10))
                            {BinopC '- (NumC 10) (BinopC '* (NumC 1) (NumC 4))}) testFds) 6)
#;(check-equal? (interp (parse '(* 3 (+ 2 7))) testFds) 27)
#;(check-exn #rx"VVQS" (lambda() (interp (BinopC '/ (NumC 1) (NumC 0)) testFds)))


;; tests on both parse and interp
#;(check-equal? (interp (parse '1) testFds) 1)
#;(check-equal? (interp (parse '{+ 2 3}) testFds) 5)
#;(check-equal? (interp (parse '{* 2 3}) testFds) 6)
#;(check-equal? (interp (parse '{* {+ 1 2} {+ 3 4}}) testFds) 21)
#;(check-equal? (interp (parse '{+ {* 1 2} {* 3 4}}) testFds) 14)
#;(check-equal? (interp (parse '{+ {* 1 {* 4 2}} {* 3 {+ 6 4}}}) testFds) 38)
#;(check-equal? (interp (parse '{- {* 3 {* 4 2}} {* 2 {+ 6 4}}}) testFds) 4)
#;(check-equal? (interp (parse '{- {/ {* 4 6} 3} {* 2 {+ 1 2}}}) testFds) 2)
#;(check-equal? (interp (parse '{/ {* 10 {* 5 2}} {* 2 {- 7 2}}}) testFds) 10)
#;(check-equal? (interp (parse '{leq0? (+ 1 4)
                                     then (* 4 5)
                                     else (- 4 (+ 1 2))}) testFds) 1)
#;(check-equal? (interp (parse '{leq0? (+ 1 -4)
                                     then (* 4 5)
                                     else (- 4 (+ 1 2))}) testFds) 20)



;; Interpret-fns takes a list of FdCs and returns the interpretation of the main function
#;(define (interp-fns [funs : (Listof FdC)] [env : ]) : Real
  (interp (AppC 'main '()) funs env)) 

#;(check-equal?  (interp-fns (parse-prog '{{def {double x} = {* 2 x}}
                            {def {main} = {double 13}}})) 26)
#;(check-equal?  (interp-fns (parse-prog '{{def {add-one x} = {+ 1 x}}
                            {def {main} = {add-one 13}}})) 14)


;; TOP LEVEL ENVIRONMENT
(define top-env 
  (bind (list 'true 'false '+ '- '* '/ '<= 'equal?) (list #t #f (PrimV '+) (PrimV '-) (PrimV '*) (PrimV '/) (PrimV '<=) (PrimV 'equal?) )))

#;(define testEnv2 
  (bind (list 'f 'true 'false) (list (CloV (list 'x) (AppC (IdC 'x) (list (IdC 'x))) '()) #t #f)))

;; top-interp takes in Sexp in VVQS language and returns the interpreted output of a real number
(: top-interp (Sexp -> Value))
;(define top-env (list (Binding '+ (PrimV '+)) (Binding '- (PrimV '-))))
(define (top-interp sexps)
  (serialize (interp (parse sexps) top-env)))


#;(check-equal?  (top-interp '{{def {double x y} = {* 2 y}}
                            {def {main} = {+ 13 15}}}) 28)

#;(check-equal?  (top-interp '{{def {double x} = {* 2 x}}
                            {def {main} = {double 13}}}) 26)

#;(check-equal?  (top-interp '{{def {add3together a b c} = {+ a {+ b c}}} 
                             {def {makeNeg x} = {* -1 x}}
                             {def {add5ThenMult2 x} = {* 2 {+ 5 x}}}
                             {def {main} = {+ {+ {add2ThenNeg 12} {add5ThenMult2 2}} {add3together 2 5 9}}}
                             {def {add2ThenNeg x} = {makeNeg {+ 2 x}}}}) 16)

(check-equal? (top-interp '{true}) "true")
(check-equal? (top-interp '{false}) "false")
(check-equal? (top-interp '{6}) "6")
;(check-equal? (top-interp '{"hi"}) "hi")  DO WE NEED TO ADD A CASE TO PARSE FOR STRINGS ??
(check-equal? (top-interp '{{x} => {x}}) "#<procedure>")
(check-equal? (top-interp '{{{x} => {+ x 1}} 3}) "4")
(check-exn #rx"VVQS" (lambda() (top-interp '{{{x} => {+ x true}} 3})))
(check-equal? (top-interp '{{{x} => {<= x 1}} 3}) "false")
(check-equal? (top-interp '{{{x} => {<= x 4}} 3}) "true")
(check-equal? (top-interp '{{{x} => {equal? x 4}} 4}) "true") 
(check-equal? (top-interp '{equal? 3 3}) "true")
(check-equal? (top-interp '{equal? 3 4}) "false")
(check-equal? (top-interp '{{{x} => {8 if x else 1}} true }) "8")
(check-equal? (top-interp '{{{x} => {8 if x else 1}} {<= 3 4} }) "8")
(check-exn #rx"VVQS" (lambda () (top-interp '{{{x} => {8 if x else 1}} {+ 3 4} })))
(check-equal? (top-interp '{{{{x} => {{- 12 5} if x else {{y} => {1 if y else 12}}}} false } true}) "1") ;; weird case. was just messing around
(check-equal? (top-interp '{{+ x y} where {[x := 5] [y := 7]}}) "12")












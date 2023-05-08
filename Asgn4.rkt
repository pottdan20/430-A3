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
(define-type Value (U Real Boolean String CloV PrimV FunV ErrorV))
(struct FunV ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Environment]) #:transparent)
(struct Binding ([s : Symbol] [v : Value]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)
(struct ErrorV ([msg : String]) #:transparent)



(define invalid-id-hash
  (hash 'where 'where
        'if 'if
        'else 'else
        '=> '=>
        '= '=
        ':= ':=))

;; takes any and returns if it is a valid-id as a bool 
(define (valid-id? [id : Any]) : Boolean
  (match id
    [(? symbol? s) (not (hash-has-key? invalid-id-hash s))]
    [other false]))
;; validates a list of ids
#;(define (valid-ids? [ids : (Listof Any)]) : Boolean
  (match ids
    ['() true]
    [(cons f r) (and (valid-id? f) (valid-ids? r))]))


(check-equal? (valid-id? "hi") false)
(check-equal? (valid-id? 'hi) true)
(check-equal? (valid-id? 'if) false)


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

(check-equal? (get-operator '+) +)
(check-equal? (get-operator '-) -)
(check-equal? (get-operator '*) *)
(check-equal? (get-operator '/) /)
(check-exn #rx"VVQS" (lambda() (get-operator '\))))
(check-exn #rx"VVQS" (lambda() (get-operator 'a)))
(check-exn #rx"VVQS" (lambda() (get-operator 'asdf)))


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
(check-exn #rx"VVQS" (lambda () (serialize (IdC 'b))))


;(define (has-repeats? [args : (Listof Symbol)] [checked : mutable hash]))

;; PARSER 

;; takes an Sexp and returns the ExprC corresponding to the Sexp, if applicable.
;; otherwise, an error is thrown
(define (parse [expr : Sexp]) : ExprC
  (match expr
   [(? real? n) (NumC n)]
   [(? string? s) (StringC s)]
   [(list expr1 'if expr2 'else expr3) (IfC (parse expr1) (parse expr2) (parse expr3))]
   [(list expr1 'where (list (list a ':= b) ...))
    (cond
      [(check-duplicates (cast a (Listof Symbol))) (error "VVQS: dulicate vars in where. got ~e" a)]
      [else (AppC (LamC (cast a (Listof Symbol)) (parse expr1))
          (cast (map (lambda (x) (parse x)) (cast b (Listof Sexp))) (Listof ExprC)))])
    ]
   [(list (list (? symbol? syms) ...) '=> expr) (cond
                                                  [(check-duplicates (cast syms (Listof Symbol)))
                                                   (error "VVQS: error -- repeat ids, got ~e" syms) ]
                                                  [else (LamC (cast syms (Listof Symbol)) (parse expr))])]
   [(? symbol? sym) (cond
                      [(valid-id? sym) (IdC (cast sym Symbol))]
                      [else (error "VVQS: error -- expected valid id, got ~e" sym)])]
                      
   [(list lam exprs ...) (AppC (parse lam) (map (lambda (x) (parse x)) exprs))]
   [other (error "VVQS: error -- expected expression, got ~e" other)]))



(check-exn #rx"VVQS" (lambda () (parse '{{{} not allowed {hiiii}}})))
(check-exn #rx"VVQS" (lambda () (parse '{- if 9}))) 
(check-exn #rx"VVQS" (lambda () (parse '{{x x} => 9})))
(check-exn #rx"VVQS" (lambda () (parse '{{9} where {[x := 7]
                                                    [x := 4]}})))
(check-equal? (parse '{{x} => 5}) (LamC (list 'x) (NumC 5)))
(check-equal? (parse '{{+ x y} where {[x := 5] [y := 7]}})
              (AppC (LamC (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 5) (NumC 7))))


;; INTERPETER
;; takes an ExprC and returns the result of the expression, if possible.
;; if not, an error is thrown
(define (interp [expr : ExprC] [env : Environment]): Value
  (match expr
    [(NumC n) n]
    [(StringC s) s]
    ;[(FdC name args body) (FunV name args body)]  
    [(LamC args body) (CloV args body env)]
    [(IfC do if else) (local ([define condition (interp if env)])
                        (match condition
                          [#t (interp do env)]
                          [#f (interp else env)]
                          [else ((error "VVQS: error -- expected boolean expression in if, got ~e" if))]))]
    [(AppC f args) (local ([define f-value (interp f env)])
                                              ;; from text book
                     (match f-value
                       [(? real? n) (error "VVQS: invaid id func call got ~e" f-value)]
                       [(CloV as bod CloEnv) (interp bod
                                              (extend-env (bind as
                                                                (map (lambda ([a : ExprC]) (interp a env)) args))
                                                         CloEnv))]
                       [(PrimV s) (match args
                                    [(cons l (cons r '())) (local ([define real-l (interp l env)]
                                                                   [define real-r (interp r env)])
                                                             (match real-l
                                                               [(? real? rl)
                                                                (match real-r
                                                                  
                                                                  [(? real? rr) (cond
                                                                                  [(and (= 0 rr) (symbol=? s '/))
                                                                                   (error "VVQS: divide by 0" )]
                                                                                  [else ((get-operator s) rl rr)])]
                                                                                  

                                                                  [other (error "VVQS: error --
                                                                   binary operator must take two reals, got ~e" args)])]
                                                               [other (error "VVQS: error --
                                                                   binary operator must take two reals, got ~e" args)]
                                                             ))]
                                    [other (error "VVQS: error -- expected two arguments, got ~e" args)])]
                       [(ErrorV msg) (define userMsg (first args))
                                     (match userMsg
                                       [(? StringC? s) (error (string-append msg (StringC-s (cast userMsg StringC))))]
                                       [other (error msg)])]  
                       [other f-value ]))];(error "VVQS: error -- called expression of invaid type" )]) 
                      
                     
                     
    [(IdC x) (env-lookup x env)]))



;;extend env extends env
;; adds new first env because lookup goes from front to back
(define (extend-env [e1 : Environment] [e2 : Environment]) : Environment 
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



(check-exn #rx"VVQS"
           (lambda() (bind '(+ -) '(1))))
(check-exn #rx"VVQS"
           (lambda() (bind '(+) '(1 2))))


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








;;interp tests 

(define testEnv1 
  (bind (list 'f 'g 'x) (list
                         (CloV (list 'y 'x) (AppC (IdC 'y) (list (IdC 'x))) '())
                         9
                         3)))
;(check-equal? (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (NumC 6))) testEnv1) 6)


(define testEnv2 
  (bind (list 'f 'true 'false) (list (CloV (list 'y 'x) (AppC (IdC 'y) (list (IdC 'x))) '()) #t #f)))

(check-equal? (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (IdC 'true))) testEnv2) #t)
(check-equal? (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (IdC 'false))) testEnv2) #f)

(check-equal? (serialize (interp (AppC (IdC 'f) (list (LamC (list 'a) (IdC 'a)) (IdC 'false))) testEnv2)) "false")

(check-exn #rx"VVQS"
           (lambda() (env-lookup 'not-there testEnv2)))
;(check-equal? (interp (AppC (IdC 'f) (list (NumC 3) (IdC 'x))) testEnv1) 3)

;(check-equal? (interp (LamC (list 'a 'b 'c) (LamC (list 'l) (IdC 'b))) testEnv1) 4)
;(check-equal? (interp (LamC (list 'a 'b 'c) (IdC 'b)) testEnv1) 5)



;; TOP LEVEL ENVIRONMENT
(define top-env 
  (bind (list 'true 'false '+ '- '* '/ '<= 'equal? 'error)
        (list #t #f (PrimV '+) (PrimV '-) (PrimV '*)
              (PrimV '/) (PrimV '<=) (PrimV 'equal?) (ErrorV "user-error: "))))

#;(define testEnv2 
  (bind (list 'f 'true 'false) (list (CloV (list 'x) (AppC (IdC 'x) (list (IdC 'x))) '()) #t #f)))

;; top-interp takes in Sexp in VVQS language and returns the interpreted output of a real number
(: top-interp (Sexp -> Value)) 
;(define top-env (list (Binding '+ (PrimV '+)) (Binding '- (PrimV '-))))
(define (top-interp sexps)
  (serialize (interp (parse sexps) top-env)))



 
(check-equal? (top-interp '{true}) "true")
(check-equal? (top-interp '{false}) "false")
;(check-equal? (top-interp '{6}) "6")
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
(check-equal? (top-interp '{{{{x} => {{- 12 5} if x else {{y} => {1 if y else 12}}}} false } true}) "1") 
(check-equal? (top-interp '{{+ x y} where {[x := 5] [y := 7]}}) "12")
(check-exn #rx"VVQS" (lambda() (top-interp '{{/ 2 0}})))
(check-exn #rx"VVQS" (lambda() (top-interp '{{- 4}}))) 
(check-exn #rx"VVQS" (lambda() (top-interp '{{- 4 "hi"}})))
(check-exn #rx"VVQS" (lambda() (top-interp '{{- "hi" 5}})))

(check-exn #rx"user-error: TEST" (lambda() (top-interp '{{error "TEST"}}))) 
(check-exn #rx"user-error: ELSE" (lambda() (top-interp '{1 if {<= 1 0} else {error "ELSE"}})))

(check-exn #rx"VVQS" (lambda() (top-interp '{3 4 5}))) 
;(parse '{3 4 5})

(check-exn #rx"user-error" (lambda() (top-interp '(((e) => (e e)) error))))

(check-equal? (top-interp '{{{{{x} => {{y} => {{z} => {+ {+ x y} z}}}} 2} 3} 4}) "9")









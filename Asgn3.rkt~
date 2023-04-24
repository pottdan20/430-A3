#lang typed/racket
(require typed/rackunit)


;; 3 write parser and interp

;; Data definitions
(define-type ExprC (U NumC BinopC Leq0 IdC AppC))
(struct NumC ([n : Real]) #:transparent)

; binop
(struct BinopC ([operator : Symbol] [l : ExprC] [r : ExprC]) #:transparent)

; conditional
(struct Leq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

; function stuff
(struct IdC ([s : Symbol]) #:transparent) ;; an ID element
(struct AppC ([fun : Symbol] [expr : ExprC]) #:transparent) ;; application of a func






;; PARSER

;; takes an Sexp and returns the ExprC corresponding to the Sexp, if applicable.
;; if not, an error is thrown
(define (parse [expr : Sexp]) : ExprC
  (match expr
     [(? real? n) (NumC n)]
     [(list (? symbol? s) l r) (BinopC s (parse l) (parse r))]
     [(list 'leq0? test 'then then 'else else) (Leq0 (parse test) (parse then) (parse else))]
     [other (error "VVQS: error -- expected expression, got ~e" other)])
  )


(check-equal? (parse '1) (NumC 1))
(check-equal? (parse '{+ 2 3}) (BinopC '+ (NumC 2) (NumC 3)))
(check-equal? (parse '{* 2 3}) (BinopC '* (NumC 2) (NumC 3)))
(check-equal? (parse '{* {+ 1 2} {+ 3 4}}) (BinopC '* (BinopC '+ (NumC 1) (NumC 2)) (BinopC '+ (NumC 3) (NumC 4))))
(check-equal? (parse '{+ {* 1 2} {* 3 4}}) (BinopC '+ (BinopC '* (NumC 1) (NumC 2)) (BinopC '* (NumC 3) (NumC 4))))

(check-equal? (parse '{hi 1 2}) (BinopC 'hi (NumC 1) (NumC 2)))  ; PARSERS ALLOWS ANY SYMBOL - FIX?

(check-exn #rx"VVQS" (lambda() (parse '{+ 4})))
(check-exn #rx"VVQS" (lambda() (parse '{+})))
(check-exn #rx"VVQS" (lambda() (parse '{})))
(check-exn #rx"VVQS" (lambda() (parse '{+ {+ 1 2}})))
(check-exn #rx"VVQS" (lambda() (parse '{* {+ 1 2}})))
(check-exn #rx"VVQS" (lambda() (parse '{* {+ 1 2} {+ 1}})))

#;(check-exn (regexp (regexp-quote "VVQS:"))
           (lambda () (parse '{- 2 3})))

(check-equal? (parse '{leq0? 1
                             then 1
                             else {- 1 1}})
              (Leq0 (NumC 1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)}))
(check-equal? (parse '{leq0? {+ 1 -2}
                             then {- 10 {+ 1 2}}
                             else {- 1 1}})
              (Leq0 (BinopC '+ (NumC 1) (NumC -2)) (BinopC '- (NumC 10) (BinopC '+ (NumC 1) (NumC 2))) {BinopC '- (NumC 1) (NumC 1)}))



;; INTERPETER

;; takes an ExprC and returns the result of the expression, if possible.
;; if not, an error is thrown
(define (interp [expr : ExprC]): Real
  (match expr
    [(NumC n) n]
    [(BinopC op l r) ((get-operator op) (interp l) (interp r))]
    [(Leq0 test then else) (cond
                             [(<= (interp test) 0) (interp then)]
                             [else (interp else)]
                             )]))

;; 3.1 Binary Arithmetic Operators
(define (get-operator [operator : Symbol]) : (-> Real Real Real)
  (match operator
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]
    [other (error "VVQS: error -- expected a valid operator (+, -, *, /), got ~e" other)]))



(check-equal? (interp (NumC 1)) 1)
(check-equal? (interp (NumC 0)) 0)
(check-equal? (interp (BinopC '+ (NumC 1) (NumC 2))) 3)
(check-equal? (interp (BinopC '* (NumC 1) (NumC 2))) 2)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 10)) (NumC 2))) 22)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 1) (NumC 10)) (BinopC '+ (NumC 1) (NumC 2)))) 33)
(check-equal? (interp (BinopC '+ (BinopC '+ (NumC 1) (NumC 10)) (BinopC '+ (NumC 1) (NumC 2)))) 14)
(check-equal? (interp (BinopC '* (BinopC '+ (NumC 0) (NumC 0)) (BinopC '+ (NumC 0) (NumC 0)))) 0)
(check-equal? (interp (BinopC '- (BinopC '+ (NumC 1) (NumC 2)) (BinopC '+ (NumC 1) (NumC 1)))) 1)
(check-equal? (interp (BinopC '/ (BinopC '+ (NumC 2) (NumC 2)) (BinopC '+ (NumC 1) (NumC 1)))) 2)
(check-equal? (interp (BinopC '/ (BinopC '* (NumC 2) (NumC 20)) (BinopC '- (NumC 7) (NumC 3)))) 10)

(check-exn #rx"VVQS" (lambda() (interp (BinopC 'hi (NumC 1) (NumC 2)))))
(check-exn #rx"VVQS" (lambda() (interp (BinopC 'a (NumC 1) (BinopC '+ (NumC 1) (NumC 2))))))
(check-exn #rx"VVQS" (lambda() (interp (BinopC '+ (NumC 1) (BinopC 'a (NumC 1) (NumC 2))))))

(check-equal? (interp (Leq0 (NumC 1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)})) 0)
(check-equal? (interp (Leq0 (NumC -1) (NumC 1) {BinopC '- (NumC 1) (NumC 1)})) 1)
(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC 2)) (NumC 1) {BinopC '- (NumC 10) (NumC 1)})) 9)
(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC -2)) (BinopC '* (NumC 10) (NumC 10)) {BinopC '- (NumC 10) (NumC 1)})) 100)
(check-equal? (interp (Leq0 (BinopC '+ (NumC 1) (NumC 2)) (BinopC '* (NumC 10) (NumC 10)) {BinopC '- (NumC 10) (BinopC '* (NumC 1) (NumC 4))})) 6)







;; tests on whole process - both parse and interp
(check-equal? (interp (parse '1)) 1)
(check-equal? (interp (parse '{+ 2 3})) 5)
(check-equal? (interp (parse '{* 2 3})) 6)
(check-equal? (interp (parse '{* {+ 1 2} {+ 3 4}})) 21)
(check-equal? (interp (parse '{+ {* 1 2} {* 3 4}})) 14)
(check-equal? (interp (parse '{+ {* 1 {* 4 2}} {* 3 {+ 6 4}}})) 38)
(check-equal? (interp (parse '{- {* 3 {* 4 2}} {* 2 {+ 6 4}}})) 4)
(check-equal? (interp (parse '{- {/ {* 4 6} 3} {* 2 {+ 1 2}}})) 2)
(check-equal? (interp (parse '{/ {* 10 {* 5 2}} {* 2 {- 7 2}}})) 10)

(check-equal? (interp (parse '{leq0? (+ 1 4)
                                     then (* 4 5)
                                     else (- 4 (+ 1 2))})) 1)
(check-equal? (interp (parse '{leq0? (+ 1 -4)
                                     then (* 4 5)
                                     else (- 4 (+ 1 2))})) 20)

(check-exn #rx"VVQS" (lambda() (interp (parse '{+ 4}))))
(check-exn #rx"VVQS" (lambda() (interp (parse '{+}))))
(check-exn #rx"VVQS" (lambda() (interp (parse '{}))))



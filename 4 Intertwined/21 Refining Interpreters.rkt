;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |21.1 Interpreting Expressions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
;; Exercise 358. Provide a structure type and a data definition for function definitions.
; BSL-fun-def is [Symbol List-of-Symbol BSL-fun-expr]
(define-struct bfd [name pl body])

;; Exercise 345. Formulate a data definition for the representation of BSL expressions based on the structure type definitions of add and mul.
; BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

;; Exercise 346. Formulate a data definition for the class of values



;; Exercise 347. Design eval-expression. The function consumes a representation of a BSL expression and computes its value.
; to which a representation of a BSL expression can evaluate.

;
(define (atom? exp)
  (or (number? exp) (string? exp) (symbol? exp)))

; Exp.v1 -> Atom
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)
(define (eval-expression exp)
  (cond
    [(atom? exp) exp]
    [else ;; primitive
       (cond
         [(add? exp) (+ (eval-expression (add-left exp)) (eval-expression(add-right exp)))]
         [(mul? exp) (* (eval-expression (mul-left exp)) (eval-expression(mul-right exp)))])]))

;; Exercise 348. Develop a data representation for Boolean BSL expressions
; BoolExpr is one of:
; - Boolean
; - (BoolPrim BoolExpr BoolExpr)

; Boolean is one of:
; - #true
; - #false

; BoolPrim is one of:
; - and
; - or
; - not

; BoolExpr -> Boolean
; consumes bexpr and computes their values
;(define (eval-bool-expression bexpr)
;  (cond
;    [(boolean? bexpr) bexpr]
;    [else ; (BoolPrim BoolExpr BoolExpr) -> Boolean
;     ...]))


;;; Figure 129: From S-expr to BSL-expr

; S-expr -> BSL-expr
; check if s follows the BSL data defination
;; Exercise 349. Create tests for parse cover all branch
(define WRONG "It comes to wrong case.")
(define LW "less")
(define MW "more")
(define OUT-OF-PRIM "out of prim")
(define +se '(+ 5 234))
(define *se '(* 32 10))
(define +*se '(+ 5 (* 7 5)))
(check-expect (parse +se) (make-add 5 234))
(check-expect (parse *se) (make-mul 32 10))
(check-expect (parse +*se) (make-add 5 (make-mul 7 5)))
(check-error (parse 'h) WRONG)
(check-error (parse "h") WRONG)
(check-error (parse '(+ 21) LW))
(check-error (parse '(+ 5 (* 7)) LW))
(check-error (parse '(* (+ 7) 5) LW))
(check-error (parse '($ 32 10)) OUT-OF-PRIM)
(check-error (parse '(+ 44 (* 55 24 21)) MW))
(check-error (parse '(* (+ 55 24 21) 44) MW))
(check-error (parse '(+ 55 24 21) MW))
(check-error (parse '(* 55 24 21) MW))
(check-error (parse (list "55" '(24 21)) MW))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(check-error (parse-sl '(+) LW))
(check-error (parse-sl '(+ 55 24 'sfd 55 66) MW))
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error LW)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error OUT-OF-PRIM)])]
      [else (error MW)])))

; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; S-expr -> ValueOrError
;  If parse recognizes s as BSL-expr, it produces s's value. Otherwise, it signals the same error as parse. 
(check-expect (interpreter-expr +*se) 40)
(check-error (interpreter-expr '(* 55 (+ 12 43 55)) MW))
(check-error (interpreter-expr '(+ ("*" 12 43 55) 55) MW))
(define (interpreter-expr s)
  (eval-expression (parse s)))
  

;; Exercise 350. What is unusual about the definition of this program with respect to the design recipe?

;; Exercise 351. Design interpreter-expr. The function accepts S-expressions. If parse recognizes them as BSL-expr, it produces their value. Otherwise, it signals the same error as parse.

;; Exercise 352. Design subst.

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr Symbol Number -> BSL-var-expr
; consumes ex x v and produces ex with all occurrences of x replaced by v
(define a12 (make-add 'fi 12))
(define ma2 (make-mul 'se (make-add 'se 'se)))
(define am2 (make-add 'se (make-mul 3 'fi)))
(check-expect (subst a12 'fi 3) (make-add 3 12))
(check-expect (subst ma2 'se 2) (make-mul 2 (make-add 2 2)))
(check-expect (subst am2 'se 2) (make-add 2 (make-mul 3 'fi)))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add
                (subst (add-left ex) x v)
                (subst (add-right ex) x v))]
    [(mul? ex) (make-mul
                (subst (mul-left ex) x v)
                (subst (mul-right ex) x v))]
    [(list? ex)
     (cond
       [(empty? ex) ex]
       [else (cons (subst (first ex) x v)
                   (subst (rest ex) x v))])]))
;; Exercise 353. Design the numeric? function.
; BSL-var-expr -> Boolean
; It determines whether a BSL-var-expr is also a BSL-expr.
(check-expect (numeric? (subst a12 'fi 3)) #t)
(check-expect (numeric? (subst ma2 'se 2)) #t)
(check-expect (numeric? (subst am2 'se 2)) #f)
(define (numeric? ve)
  (cond
    [(number? ve) #t]
    [(symbol? ve) #f]
    [(add? ve) (and
                (numeric? (add-left ve))
                (numeric? (add-right ve)))]
    [(mul? ve) (and
                (numeric? (mul-left ve))
                (numeric? (mul-right ve)))]
    ;[else #f]
    ))
;; Exercise 354. part1 Design eval-variable.
; BSL-var-expr -> ValueOrError
; consumes a BSL-var-expr and determines its value if numeric? yields true for the input
(check-expect (eval-variable (subst a12 'fi 3)) 15)
(check-error (eval-variable (subst am2 'se 2)) WRONG)
(define (eval-variable ve)
  (if (numeric? ve)
      (eval-expression ve)
      (error WRONG)))
;; Exercise 354. part2
; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; BSL-var-expr AL -> ValueOrError
; for item in da { subst ve item;
;     if (numeric? result){retrun value}
;     else error}
(define fi1 '(fi 1))
(define se2 '(se 2))
(define ra199 '(ra 199))
(define al12199 (list fi1 se2 ra199))
(define ve1 (make-add 'se (make-mul 'ra 'fi)))
(define ve2 (make-add 2 (make-mul 3 4)))
(define ve3 (make-add 'fi (make-mul 3 4)))
(check-expect (eval-variable* ve3 al12199) 13)
(check-expect (eval-variable* ve2 al12199) 14)
(check-expect (eval-variable* ve1 al12199) 201)
(define (eval-variable* ve da)
  (eval-variable (subst* ve da)))

(define (subst* ve da)
  (cond
    [(empty? da) ve]
    [else
     (local
       [(define item (first da))]
       (subst*
        (subst ve (first item) (second item))
        (rest da)))]))
;;Exercise 355.
; BSL-var-expr AL -> Number
; Just like eval-variable* but using assq to look up the value of x in the association list.
(check-expect (eval-var-lookup ve1 al12199) 201)
(check-error (eval-var-lookup (make-add 'bbb (make-mul 3 4)) al12199) WRONG)


(define (eval-var-lookup ex da)
  (local
    [(define (helper e)
      (cond
        [(number? e) e]
        [(symbol? e)
         (local
           [(define result (assq e da))]
           (if (boolean? result)
               (error WRONG)
               (second result)))]
        [(add? e)
         (make-add
          (helper (add-left e))
          (helper (add-right e)))]
        [(mul? e)
         (make-mul
          (helper (mul-left e))
          (helper (mul-right e)))]))]
    (eval-variable (helper ex))))
;; TODO: abstract??

;;; 21.3 Interpreting Functions
;; Exercise 356.
; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – (Function BSL-fun-expr BSL-fun-expr)
; - (list Symbol BSL-fun-expr)
; Funcion is one of:
; - make-add
; - make-mul

; (k (+ 1 1))  (list 'k (make-add 1 1))
; (* 5 (k (+ 1 1)))  (make-mul 5 ('k (make-add 1 1)))
; (* (i 5) (k (+ 1 1)))  (make-mul ('i 5) ('k (make-add 1 1)))
;; Exercise 357. Design eval-definition1.
; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Value
; evaluates expression. evalutes the ar
; If eval-definition1 encounters a variable, it signals the same error as eval-variable from exercise 354.
; It also signals an error for function applications that refer to a function name other than f.
(check-expect (eval-definition1 (list 'k (make-add 1 1)) 'k 'x (make-add 'x 1))
               3)
(check-expect (eval-definition1 (make-mul (list 'k (make-add 1 1)) 1) 'k 'x (make-add 'x 1))
               3)
(check-error (eval-definition1 (list 'k (make-add 'q 1)) 'k 'x (make-add 'x 1)) WRONG)
(check-error (eval-definition1 (list 'q (make-add 1 1)) 'k 'x (make-add 'x 1)) WRONG)
(check-error (eval-definition1 (list 'k (make-add 1 1) 'k) 'k 'x (make-add 'x 1)) WRONG)

(define (eval-definition1 ex f x b)
  (local
    [(define (eval exp)
       (cond
         [(number? exp) exp]
         [(symbol? exp) (error WRONG)]
         [(add? exp) (+ (eval(add-left exp)) (eval(add-right exp)))]
         [(mul? exp) (* (eval(mul-left exp)) (eval(mul-right exp)))]
         [(list? exp)
          (cond
            [(= (length exp) 2)
             (local
               [(define fi (first exp))
                (define se (second exp))]
               (if (and (symbol? fi) (symbol=? fi f))
                   (local ((define value (eval-definition1 se f x b))
                           (define plugd (subst b x value)))
                     (eval-definition1 plugd f x b))
                   (error WRONG)))]
            [else (error WRONG)])]))
     ]
    (eval ex)))
  

;; Exercise 358. Provide a structure type and a data definition for function definitions.
; BSL-fun-def is [Symbol List-of-Symbol BSL-fun-expr]
; (define-struct bfd [name pl body])
(define f (make-bfd 'f '(x) (make-add 3 'x)))
(define g (make-bfd 'g '(y) (list 'f (make-mul 2 'y))))
(define h (make-bfd 'h '(v) (make-add (list 'f 'v) (list 'g 'v))))
(define da-fgh (list f g h))
; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'b) WRONG)
(define (lookup-def da f)
  (cond
    [(empty? da) (error WRONG)]
    [else (local
            [(define fi (first da))
             (define re (rest da))]
            (if (equal? (bfd-name fi) f)
                fi
                (lookup-def re f)))]))
; Exercise 359. Design eval-function*.
; BSL-fun-expr BSL-fun-def* -> Value

(check-expect (eval-function* (list 'f 12) da-fgh) 15)
(check-expect (eval-function* (list 'g 1) da-fgh) 5)
(check-expect (eval-function* (list 'h 1) da-fgh) 9)
(check-error (eval-function* (list 'b 12) da-fgh) WRONG)
(define (eval-function* ex da)
  (local
    [(define (eval exp)
       (cond
         [(number? exp) exp]
         [(add? exp) (+ (eval(add-left exp)) (eval(add-right exp)))]
         [(mul? exp) (* (eval(mul-left exp)) (eval(mul-right exp)))]
         [(list? exp)
          (local
            [(define sy (first exp))
             (define in (second exp))
             (define fd (lookup-def da sy))
             (define params (bfd-pl fd))
             (define body (bfd-body fd))
             (define value (eval-function* in da))
             (define plugd (subst body (first params) value))]
            (eval-function* plugd da))]))]
    (eval ex)))
; TODO: 梳理整个parse和evalue过程逻辑
;;; 21.4 Interpreting Everything
; TODO: read 21.4
(define close-to-pi 3.14)
 
(define (area-of-circle r)
  (* close-to-pi (* r r)))
 
(define (volume-of-10-cylinder r)
  (* 10 (area-of-circle r)))
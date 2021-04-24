;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |19.3 S-expressions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol 

;; Exercise 316. Define the atom? function.
(check-expect (atom? 1) #t)
(check-expect (atom? "b") #t)
(check-expect (atom? 'sadf ) #t)
(check-expect (atom? +) #f)
(define (atom? i)
  (or (number? i)
      (string? i)
      (symbol? i)))

;; Sample Problem Design the function count, which determines how many times some symbol occurs in some S-expression.
; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(check-expect (count '(((1 world) "h" hello) hello) 'hello) 2)
(define (count sexp sy)
  (local
    [(define (count-sl sl)
       (cond
         [(empty? sl) 0]
         [else (+ (count (first sl) sy)
                  (count-sl (rest sl)))]))
     (define (count-atom at)
       (cond
         [(number? at) 0]
         [(string? at) 0]
         [(symbol? at) (if (symbol=? at sy) 1 0)]))
     ]
  (cond
    [(atom? sexp) (count-atom sexp)]
    [else (count-sl sexp)])))

;; Exercise 318. Design depth. The function consumes an S-expression and determines its depth.
; S-expr -> N
(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)
(define (depth sexp)
  (local
    [(define (helper sexp n)
       (local
         [(define (depth-sl sl)
            (cond
              [(empty? sl) n]
              [else (max (helper (first sl) (+ 1 n))
                         (depth-sl (rest sl)))]))
          (define (depth-atom at)
            (+ n 1))
          ]
         (cond
           [(atom? sexp) (depth-atom sexp)]
           [else (depth-sl sexp)])))]
    (helper sexp 0))
  )

;; Exercise 319. Design substitute.
; S-expr Symbol Symbol -> S-expr
; s with all occurrences of old replaced by new
(check-expect (substitute 'world 'hello 'world) 'world)
(check-expect (substitute 'hello 'hello 'world) 'world)
(check-expect (substitute '(((1 world) "h" hello) hello) 'hello 'world)
              '(((1 world) "h" world) world))
(define (substitute sexp sy-o sy-n)
  (local
    [(define (sub-sl sl)
       (cond
         [(empty? sl) '()]
         [else (cons (substitute (first sl) sy-o sy-n)
                    (sub-sl (rest sl)))]))
     (define (sub-atom at)
       (cond
         [(number? at) at]
         [(string? at) at]
         [(symbol? at) (if (symbol=? at sy-o) sy-n at)]))]
    (cond
      [(atom? sexp) (sub-atom sexp)]
      [else (sub-sl sexp)])))

;; Exercise 320.
; An S-expr is one of: 
; – Number
; – String
; – Symbol 
; – List-of-S-expr

;; Sample Problem Design the function count, which determines how many times some symbol occurs in some S-expression.
; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)
(check-expect (count.v2 '(((1 world) "h" hello) hello) 'hello) 2)
(define (count.v2 sepr sy)
  (local
    [(define (helper sr)
       (cond
         [(number? sr) 0]
         [(string? sr) 0]
         [(symbol? sr) (if (symbol=? sr sy) 1 0)]
         [else (foldl + 0 (map helper sr))]))]
    (helper sepr)))
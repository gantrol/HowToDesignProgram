;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 25_Non-standard_Recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 25.1 Recursion without Structure
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time

; [List-of 1String] and N are on equal footing
; (Processing Two Lists Simultaneously: Case 2)
(define (bundle s n)
  (cond
    [(or (empty? s) (zero? n)) '()]
    [else (cons (implode (take s n)) (bundle (drop s n) n))]))
(check-expect (bundle '() 0) '())
(check-expect (bundle '() 2) '())
; Exercise 421. Here is the test case for it.
; The test case produces '() because the length of strings are 0.
(check-expect (bundle '("a" "b" "c") 0) '())
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
;; helper function from `Exercise 395. `
; [X] [List X] N -> [List X]
; produces the first n items from l or all of l if it is too short.
(define (take l n)
  (cond
    [(or (empty? l) (= n 0)) '()]
    [else
     (cons (first l) (take (rest l) (sub1 n)))]))
; [X] [List X] N -> [List X]
; with the first n items removed or just '() if l is too short.
(define (drop l n)
  (cond
    [(empty? l) '()]
    [(= n 0) l]
    [else
     (drop (rest l) (sub1 n))]))

;; Exercise 422. Define the function list->chunks.
; [List-of 1String] N -> [List-of [List-of 1String]]
; consumes a list l of arbitrary data and a natural number n;
; result is a list of list chunks of size n
(define (list->chunks l n)
  (cond
    [(or (empty? l) (zero? n)) '()]
    [else (cons (take l n)
                (list->chunks (drop l n) n))]))
(check-expect (list->chunks '() 0) '())
(check-expect (list->chunks '() 2) '())
(check-expect (list->chunks '("a", "b", "c") 0) '())
(check-expect (list->chunks (explode "abcdefg") 3)
              '(("a" "b" "c") ("d" "e" "f") ("g")))
; Use list->chunks to define bundle via function composition.
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundleC s n)
  (chunks->l (list->chunks s n)))
(check-expect (bundleC '() 0) '())
(check-expect (bundleC '() 2) '())
(check-expect (bundleC '("a" "b" "c") 0) '())
(check-expect (bundleC (explode "abcdefg") 3)
              (list "abc" "def" "g"))
; [List-of [List-of 1String]] -> [List-of String]
(define (chunks->l cs)
  (cond
    [(empty? cs) '()]
    [else (cons (implode (first cs))
                (chunks->l (rest cs)))]))
(check-expect (chunks->l '(("a" "b" "c") ("d" "e" "f") ("g")))
              '("abc" "def" "g"))
; [List-of 1String] -> String (implode

;; Exercise 423. Define partition.
; String N -> [List-of [List-of 1String]]
(define (partition s n)
  (local [(define len (string-length s))]
    (cond
      [(or (zero? n) (zero? len)) '()]
      [(< len n)
       (cons (explode s) '())]
      [else (cons
             (explode (substring s 0 n))
             (partition (substring s n) n))])))
(check-expect (partition "" 0) '())
(check-expect (partition "" 2) '())
(check-expect (partition "abc" 0) '())
(check-expect (partition "abcdefg" 3)
              '(("a" "b" "c") ("d" "e" "f") ("g")))

;; 25.2 Recursion that Ignores Structure
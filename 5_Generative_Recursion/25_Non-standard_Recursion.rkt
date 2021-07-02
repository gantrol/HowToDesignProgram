;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 25_Non-standard_Recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 25.1 Recursion without Structure
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define NO_SENCE "input makes no sense")
; [List-of 1String] and N are on equal footing
; (Processing Two Lists Simultaneously: Case 2)
(define (bundle s n)
  (cond
    [(and (empty? s) (zero? n)) (error NO_SENCE)]
    [(or (empty? s) (zero? n)) '()]
    [else (cons (implode (take s n)) (bundle (drop s n) n))]))

; trivially solvable
; a case matches a result
(check-expect (bundle '() 0) '())
(check-expect (bundle '() 2) '())
; Exercise 421. Here is the test case for it.
; The test case produces error because the input make no sense.
(check-error (bundle '("a" "b" "c") 0) NO_SENCE)

; non-trivially solvable
; Breaking the source problem into two and cons the result
; The new problem is trivially solvable
; need current list and number(constant) of original problem
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

;;; 25.2 Recursion that Ignores Structure
;; Exercise 424. Draw a quick-sort diagram like the one in figure 152 for (list 11 9 2 18 12 14 4 1).
;                     '(11 9 2 18 12 14 4 1)
;                '(9 2 4 1)     11       '(18 12 14)
;        '(2 4 1)     9    '()       '(12 14)   18  '()
;    '(1)    2   '(4)              '()  12 '(14)
; '()  1 '()  '()  4 '()                 '()  14 '()
;     '(1)       '(4)                       '(14)
;         '(1 2 4)                      '(12 14)
;            '(1 2 4 9)             '(12 14 18)
;
;; Exercise 425.
;; Exercise 426. for ex424 as example, 6; it count on the input...
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon? smaller than pre one
(check-expect (quick-sort<  (list 11 9 2 18 12 14 4 1)) '(1 2 4 9 11 12 14 18))
(check-expect (quick-sort<  (list 11 11 1)) '(1 11 11))
; trivially solvable?
(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< '(11)) '(11))
(define (quick-sort< alon)
  (cond
    [(or (empty? alon) (empty? (rest alon))) alon]
    [else (local [(define pivot (first alon))]
            ;; Exercise 435. give filters (rest
            (append (quick-sort< (filters (rest alon) pivot <=))
                  (list pivot)
                  (quick-sort< (filters (rest alon) pivot >))))]))
;; Exercise 428. <= instead of <
;; Exercise 429. Use filter to define smallers and largers. 
; [List-of Number] Number [Number Number -> Boolean] -> [List-of Number]
; filter numbers which cmp with n is true in alone
;; Exercise 434. n in here cannot be zero
(define (filters alon n cmp)
  (cond
    [(empty? alon) '()]
    [else (if (cmp (first alon) n)
              (cons (first alon) (filters (rest alon) n cmp))
              (filters (rest alon) n cmp))]))
(check-expect (filters '(11 22 33 44) 22 >) '(33 44))
;; Exercise 427. 
(define THRESHOLD 20)

(define (qsort< alon)
  (cond
   [(or (empty? alon) (empty? (rest alon))) alon]
   [(> (length alon) THRESHOLD) (quick-sort< alon)]
   [else (sort< alon)]))
; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon) (sort< (rest alon)))]))
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(check-expect (qsort<  (list 11 9 2 18 12 14 4 1)) '(1 2 4 9 11 12 14 18))
(check-expect (qsort<  (list 11 9 2 18 12 14 4 1 3 5 6 7 8 10 13 15 16 17 18 19 20)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 18 19 20))
(check-expect (qsort< '()) '())
(check-expect (qsort< '(11)) '(11))

;; Exercise 431. see above


;; Exercise 432.  No interesting to solve it






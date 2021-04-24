;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |19.5 Project：BSTs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)
(define abt (make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE)))
     
(define abt2 (make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE))

;; Exercise 322.
; BT Number -> Boolean
; determines whether a given number occurs in given BT
(check-expect (contains-bt? abt 87) #f)
(check-expect (contains-bt? abt 24) #t)
(check-expect (contains-bt? abt 15) #t)
(check-expect (contains-bt? abt2 87) #t)
(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #f]
    [else
     (or (if (= n (node-ssn bt)) #t #f)
         (contains-bt? (node-left bt) n)
         (contains-bt? (node-right bt) n))]))
;; Exercise 323.
; BT Number -> Boolean
; search a given ssn occurs in given BT, return name if exist else #f
(check-expect (search-bt abt 87) #f)
(check-expect (search-bt abt 24) 'i)
(check-expect (search-bt abt 15) 'd)
(check-expect (search-bt abt2 87) 'h)
(define (search-bt bt n)
  (cond
    [(no-info? bt) #f]
    [else
     (if (= n (node-ssn bt))
         (node-name bt)
         (local
           [(define le (search-bt (node-left bt) n))
            (define ri (search-bt (node-right bt) n))]
           (if (boolean? le)
               (if (boolean? ri)
                   #f
                   ri)
               le)))]))

; BST
; NONE is always a BST.
; (make-node ssn0 name0 L R) is a BST if
;    L is a BST,
;    R is a BST,
;    all ssn fields in L are smaller than ssn0,
;    all ssn fields in R are larger than ssn0.

; BST
;    4
;  2   6
; 1 3 5 7
(define n1 (make-node 1 "hel" NONE NONE))
(define n3 (make-node 3 "hel" NONE NONE))
(define n2 (make-node 2 "hel" n1 n3))
(define n5 (make-node 5 "hel" NONE NONE))
(define n7 (make-node 7 "hel" NONE NONE))
(define n6 (make-node 6 "hel" n5 n7))
(define n4 (make-node 4 "hel" n2 n6))

;; Exercise 324. Design the function inorder.
; BT -> List-of-Number
; output the sequence of all the ssn numbers in the tree
; as they show up from left to right when looking at a tree drawing.
(check-expect (inorder n1) '(1))
(check-expect (inorder n7) '(7))
(check-expect (inorder n2) '(1 2 3))
(check-expect (inorder n4) '(1 2 3 4 5 6 7))
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else
     (append (inorder(node-left bt))
             (list (node-ssn bt))
             (inorder (node-right bt)))]))
; Exercise 325
; BST Number -> StringOrNONE
; If the tree contains a node whose ssn field is n, the function produces the value of the name field in that node.
(check-expect (search-bst n4 5) (node-name n5))
(check-expect (search-bst n2 3) (node-name n3))
(check-expect (search-bst n2 8) NONE)
(define (search-bst bst n)
  (cond
    [(no-info? bst) NONE]
    [else
     (local
       [(define bssn (node-ssn bst))]
       (cond
         [(= n bssn) (node-name bst)]
         [(< n bssn) (search-bst (node-left bst) n)]
         [(> n bssn) (search-bst (node-right bst) n)]))]))

; Exercise 326
; BST Number Symbol -> BST
; It consumes a BST B, a number N, and a symbol S. It produces a BST that is just like B and that in place of one NONE subtree contains the node structure
(define n2+4 (make-node 2 "hel" n1 (make-node 3 "hel" NONE (make-node 4 'four NONE NONE))))
(define n6+4 (make-node 6 "hel" (make-node 5 "hel" (make-node 4 'four NONE NONE) NONE) n7))
(define n2+2 (make-node 2 'two n1 n3))
(check-expect (create-bst n2 4 'four) n2+4)
(check-expect (create-bst n2 2 'two) n2+2)
(check-expect (create-bst n6 4 'four) n6+4)
(define (create-bst B N S)
  (cond
    [(no-info? B) (make-node N S NONE NONE)]
    [else
     (local
       [(define bssn (node-ssn B))]
       (cond
         [(= N bssn) (make-node N S
                                (node-left B)
                                (node-right B))]
         [(< N bssn) (make-node bssn (node-name B)
                                (create-bst (node-left B) N S)
                                (node-right B))]
         [(> N bssn) (make-node bssn (node-name B)
                                (node-left B)
                                (create-bst (node-right B) N S))]))]))

; Exercise 327
; [List-of [List Number Symbol]] -> BST
; consumes lolnn and produces a B by repeatedly applying create-bst
(check-expect (create-bst-from-list '((5 "hel") (3 "hel") (1 "hel") (7 "hel") (6 "hel") (2 "hel") (4 "hel")) )n4)
(define (create-bst-from-list lolnn)
  (local
    [(define fn (first lolnn))
     (define f1 (first fn))
     (define f2 (second fn))
     ]
    (cond
      [(empty? (rest lolnn)) (create-bst NONE f1 f2)]
      [else
       (create-bst
        (create-bst-from-list
         (rest lolnn))
        f1
        f2)])))

     ;;;;;;;;;; 空值要怎么考虑呢？

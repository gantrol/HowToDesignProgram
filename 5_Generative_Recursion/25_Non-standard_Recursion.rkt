(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))

; [List-of 1String] and N are on equal footing
; (Processing Two Lists Simultaneously: Case 2)
(define (bundle s n)
  (cond
    [(and (empty? s) (zero? n)) (...)]
    [else (... s ... n ... (bundle (rest s) (sub1 n)))]))


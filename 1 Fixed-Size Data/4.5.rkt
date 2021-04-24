;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define H 200)
(define W 100)
(define half-W (/ W 2))
(define h 20)
(define w 10)
(define half-h (/ h 2))
(define origin-H (- H half-h))

(define BACKG (empty-scene W H))
(define ROCKET (rectangle w h "solid" "red"))

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect
 (show "resting")
 (place-image ROCKET half-W origin-H BACKG))
 
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              half-W (* 3/4 W)
              (place-image ROCKET half-W origin-H BACKG)))
(check-expect
 (show "hello")
 (place-image (text "error" 20 "red")
              half-W (* 3/4 W)
              (place-image ROCKET half-W origin-H BACKG)))
(check-expect
 (show 8)
 (place-image ROCKET half-W (- origin-H (* 8 3)) BACKG))

(define (show x)
    (cond
      [(string? x)
       (cond
         [(string=? "resting" x) (place-rocket origin-H)]
         [else (place-image (text "error" 20 "red")
              half-W (* 3/4 W)
              (place-rocket origin-H))] 
         )]
      [(< x 0)
       (place-image (text (number->string x) 20 "red")
              half-W (* 3/4 W)
              (place-rocket origin-H))
       ]
      [(>= x 0)
       (place-rocket (- origin-H (* x 3)))]))
(define (place-rocket height)
  (place-image ROCKET half-W height BACKG))
; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting
(check-expect (launch "resting" "k") "resting")
(check-expect (launch "resting" " ") -3)
(check-expect (launch "rest" " ") "error")
(check-expect (launch 10 " ") 10)
(define (launch x ke)
  (cond
      [(string? x)
       (cond
         [(string=? "resting" x)
          (cond
            [(string=? " " ke) -3]
            [else x]
         )]
         [else "error"]
         )]
      
      [else x]))
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -2) -1)
(check-expect (fly 0) 1)
(check-expect (fly 3) 4)
(check-expect (fly "r") "error")
;(check
(define (fly x)
  (cond
    [(string? x)
      (cond
        [(string=? "resting" x) x]
        [else "error"]
        )]
    [(< x 0) (+ x 1)]
    [(>= x 0) (+ x 1)]
    [else "error"])
      )

(define (main x)
  (big-bang x
    [on-tick fly 1]
    [to-draw show]
    [on-key launch]
    ))

(main "resting")
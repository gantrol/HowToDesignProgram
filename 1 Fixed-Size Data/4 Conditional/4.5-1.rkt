;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4.5-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define H 200)
(define W 100)
(define YDELTA 10)
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
 (show 8)
 (place-image ROCKET half-W (- 8 half-h) BACKG))

; LRCD -> Image
; renders the state as a resting or flying rocket 
(define (show x)
  (cond
    [(string? x) (place-rocket H)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  half-W (* 3/4 W)
                  (place-rocket H))]
    [(>= x 0) (place-rocket x)]
    ))

(define (place-rocket height)
  (place-image ROCKET half-W (- height half-h) BACKG))
; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
  (cond
    [(string? x)
     (cond
       [(string=? "resting" x)
        (if (string=? " " ke) -3 x)]
       )]
    [(<= -3 x -1) x]
    [(>= x 0) x]))
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -1) origin-H)
;(check-expect (fly 0) origin-H)
(check-expect (fly 10) (- 10 YDELTA))
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -2) (+ x 1)]
    [(= x -1) origin-H]
    [(>= x 0) (- x YDELTA)]
    ))
(define (is-dispear? x)
  (cond
    [(string? x) #false]
    [(= x (* -1 half-h)) #true]
    [else #false]))
; LRCD -> LRCD
(define (main1 s)
  (big-bang s
    [on-tick fly 0.1]
    [to-draw show]
    [on-key launch]
    [stop-when is-dispear?]
    ))
(main1 "resting")
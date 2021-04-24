;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4.6TraficLight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define r/2 5)
(define r (* r/2 2))
(define W (+ (* (* r 2) 3) (* r/2 2)))
(define H (* 5 r/2))
(define H/2 (/ H 2))
(define 1p (make-posn (+ r (/ r/2 2)) H/2))
(define 2p (make-posn (+ (* 3 r) r/2) H/2))
(define 3p (make-posn (+ (* 5 r) (* r/2 1.5)) H/2))
(define BG (empty-scene W H))

(define red 0)
(define green 1)
(define yellow 2)
; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next red) green)
(check-expect (tl-next green) yellow)
(check-expect (tl-next yellow) red)
(define (tl-next cs)
  (cond
    [(equal? red cs) green]
    [(equal? green cs) yellow]
    [(equal? yellow cs) red]
    )
  )
; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs
;(define (tl-next-numeric cs) (modulo (+ cs 1) 3))
 
; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render red) (light-one-bulb red))
(check-expect (tl-render green) (light-one-bulb green))
(check-expect (tl-render yellow) (light-one-bulb yellow))
(define (tl-render current-state)
  (light-one-bulb current-state))
(define (light-one-bulb color)
  (place-images
            (list (circle r (if (= red color) "solid" "outline") "red")
                  (circle r (if (= green color) "solid" "outline") "green")
                  (circle r (if (= yellow color) "solid" "outline") "yellow"))
            (list 1p 2p 3p)
            BG))
(define (main ws)
  (big-bang ws
    [on-tick tl-next 3]
    [to-draw tl-render]
    ))
(main red)
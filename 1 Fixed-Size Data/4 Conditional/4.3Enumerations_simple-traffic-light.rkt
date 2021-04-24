;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname simple-traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define TICK-RATE 1)
(define LIGHT-RADIUS 20)
(define LIGHT-WIDTH (* 2 LIGHT-RADIUS))
(define BACKGROUND
  (rectangle LIGHT-WIDTH LIGHT-WIDTH "outline" "white"))

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next  "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))
; 0\1\2 -> TrafficLight
; return color for number
; WorldState: data representing the current world (cw)
 
; WorldState -> Image
; place the next color of light to background
(check-expect (render "yellow") (overlay (circle LIGHT-WIDTH "solid" "yellow") BACKGROUND))
(define (render cw)
  (overlay (circle LIGHT-WIDTH "solid" cw) BACKGROUND))

; WorldState -> WorldState
; change to next light
(check-expect (clock-tick-handler "red") "green")
(check-expect (clock-tick-handler "green") "yellow")
(check-expect (clock-tick-handler  "yellow") "red")
(define (clock-tick-handler cw)
  (traffic-light-next cw)
  )

(define (main ws)
  (big-bang ws
    [on-tick clock-tick-handler TICK-RATE]
    [to-draw render]
  )
)
(main "red")


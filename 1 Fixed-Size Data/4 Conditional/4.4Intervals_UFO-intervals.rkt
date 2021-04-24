;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname UFO-intervals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; A WorldState is a Number.
; interpretation number of pixels between the top and the UFO
 
(define WIDTH 1000) ; distances in terms of pixels 
(define HALF-WIDTH (/ WIDTH 2))
(define HEIGHT 1000)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay
             (circle 10 "solid" "black")
             (ellipse 60 30 100 "black"))
)
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render/status]))
 
; WorldState -> WorldState
; computes next location of UFO 
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; places UFO at given height into the center of MTSCN
(check-expect (render 11) (place-image UFO HALF-WIDTH 11 MTSCN))
(define (render y)
  (place-image UFO HALF-WIDTH y MTSCN))

; WorldState -> Image
; adds a status line to the scene created by render  
 
(check-expect (render/status 42)
              (place-image (text "closing in" 11 "orange")
                           20 20
                           (render 42)))
 
(define (render/status y)
  (place-image
    (cond
      [(<= 0 y CLOSE)
       (text "descending" 11 "green")]
      [(and (< CLOSE y) (<= y HEIGHT))
       (text "closing in" 11 "orange")]
      [(> y HEIGHT)
       (text "landed" 11 "red")])
    20 20
    (render y)))

(main 13)
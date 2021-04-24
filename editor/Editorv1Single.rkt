;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Editorv1Single) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "./strHelpers.rkt")
(define TEXT_SIZE 16)
(define LINE_HEIGHT (* TEXT_SIZE 1.5))
(define LINE_WIDTH 100)
(define TEXT_COLOR "black")
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t
;
; 

; Editor -> Image
; render an editor to image which contains text and cursor in posß
(define (render-editor editor)
  (overlay/align "left" "center"
                 (beside
                  (text (editor-pre editor)
                        TEXT_SIZE TEXT_COLOR)
                  (rectangle 1 LINE_HEIGHT "solid" "red")
                  (text (editor-post editor)
                        TEXT_SIZE TEXT_COLOR))
                 (empty-scene  LINE_WIDTH LINE_HEIGHT)))
(render-editor (make-editor "Hello, " "gH"))

; Editor, KeyEvent -> Editor
;
; KeyEvent: left, right, 1String, else
; - left: if left text equals "" return ed; else move one step to left
; - right: if right text equals "" return ed; else move one step to right
; - 1String
;   - \b: remove a last from pre string
;   - \r & \t: ignore
;   - else: append the string to pre string *UNTIL TEXT WIDTH IS OVER*
; - else: ed
; TODO: where is int??
(define (edit ed ke)
  
  (cond
    [(= (string-length ke) 1)
     (cond
       [(string=? "\b" ke)
        (make-editor (string-remove-last (editor-pre ed)) (editor-post ed))]
       [(>= 
          (image-width (text
                      (string-append (editor-pre ed) (editor-post ed))
                      TEXT_SIZE TEXT_COLOR))
          LINE_WIDTH)
          ;(string-length (string-append (editor-pre ed) (editor-post ed))) 
        ed]
       [(or (string=? "\t" ke) (string=? "\r" ke)) ed]
       [else (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]
       )]
    [(string=? "left" ke)
     [cond
       [(= (string-length (editor-pre ed)) 0) ed]
       [else (make-editor (string-remove-last (editor-pre ed))
                          (string-append (string-last (editor-pre ed))
                                         (editor-post ed)))]]]
    [(string=? "right" ke)
     [cond
       [(= (string-length (editor-post ed)) 0) ed]
       [else (make-editor (string-append (editor-pre ed) (string-first (editor-post ed)))
                          (string-rest (editor-post ed)))]
       ]]
    [else ed]
    ))

(define Middle (make-editor "Hello, " "GH"))
(define MiddleLeft (make-editor "Hello," " GH"))
(define LeftEnd (make-editor "" "Hello, GH"))
(define LeftEndRight (make-editor "H" "ello, GH"))
(define RightEnd (make-editor "Hello, GH" ""))
(define Empty (make-editor "" ""))
(define Max (make-editor (replicate LINE_WIDTH "i") ""))
(check-expect (edit Middle "left") MiddleLeft)
(check-expect (edit LeftEnd "left") LeftEnd)
(check-expect (edit LeftEnd "right") LeftEndRight)
(check-expect (edit RightEnd "right") RightEnd)
(check-expect (edit RightEnd "\t") RightEnd)
(check-expect (edit MiddleLeft "\t") MiddleLeft)
(check-expect (edit LeftEnd "\r") LeftEnd)
(check-expect (edit Middle "\r") Middle)
(check-expect (edit (make-editor "H" "llo, GH") "e") (make-editor "He" "llo, GH"))
(check-expect (edit Middle "\b") (make-editor "Hello," "GH"))
(check-expect (edit Empty "\b") Empty)
(check-expect (edit Middle "\b") (make-editor "Hello," "GH"))
(check-expect (edit Middle "up") Middle)
(check-expect (edit Max "a") Max)

; Main World
; Editor -> ?
(define (run ed)
  (big-bang ed
    [to-draw render-editor]
    [on-key edit]
    ))
;(run (make-editor "" ""))
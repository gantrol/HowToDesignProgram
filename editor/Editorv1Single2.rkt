;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Editorv1Single2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "./strHelpers.rkt")
(define TEXT_SIZE 16)
(define LINE_HEIGHT (* TEXT_SIZE 1.5))
(define LINE_WIDTH 100)
(define TEXT_COLOR "black")
; An Editor is a structure:
;   (make-editor String int)
; interpretation (make-editor s x) describes an editor
; whose visible text is string with 
; the cursor displayed between (substring s 0 x) and (substring s x)
(define-struct editor [str x])
(define (pre editor)
  (substring (editor-str editor) 0 (editor-x editor)))
(define (post editor)
  (substring (editor-str editor) (editor-x editor)))
; Editor -> Image
; render an editor to image which contains text and cursor in pos
(define (render editor)
  (overlay/align "left" "center"
                 (beside
                  (text (pre editor)
                        TEXT_SIZE TEXT_COLOR)
                  (rectangle 1 LINE_HEIGHT "solid" "red")
                  (text (post editor)
                        TEXT_SIZE TEXT_COLOR))
                 (empty-scene  LINE_WIDTH LINE_HEIGHT)))
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
(define Middle (make-editor "Hello, GH" 6))
(define MiddleLeft (make-editor "Hello, GH" 5))
(define LeftEnd (make-editor "Hello, GH" 0))
(define LeftEndRight (make-editor "Hello, GH" 1))
(define RightEnd (make-editor "Hello, GH" 9))
(define Empty (make-editor "" 0))
(define Max (make-editor (replicate LINE_WIDTH "i") LINE_WIDTH))
(check-expect (edit Middle "left") MiddleLeft)
(check-expect (edit LeftEnd "left") LeftEnd)
(check-expect (edit LeftEnd "right") LeftEndRight)
(check-expect (edit RightEnd "right") RightEnd)
(check-expect (edit RightEnd "\t") RightEnd)
(check-expect (edit MiddleLeft "\t") MiddleLeft)
(check-expect (edit LeftEnd "\r") LeftEnd)
(check-expect (edit Middle "\r") Middle)
(check-expect (edit (make-editor "Hllo, GH" 1) "e") (make-editor "Hello, GH" 2))
(check-expect (edit Middle "\b") (make-editor "Hello GH" 5))
(check-expect (edit Empty "\b") Empty)
(check-expect (edit LeftEnd "\b") LeftEnd)
(check-expect (edit Middle "up") Middle)
(check-expect (edit Max "a") Max)
(define (edit ed ke)
  (cond
    [(= 1 (string-length ke))
     (cond
       [(string=? ke "\b")
        (make-editor
         (string-append
          (string-remove-last (pre ed)) (post ed))
         (max 0 (- (editor-x ed) 1)))]
       [(string=? ke "\r") ed]
       [(>= (image-width (text (editor-str ed) TEXT_SIZE TEXT_COLOR))
         LINE_WIDTH)
        ed]
       [(string=? ke "\t") ed]
       [else (make-editor
              (string-append
               (pre ed) ke (post ed))
              (+ (editor-x ed) 1))])
     ]
    [(string=? ke "left") (if (<= (editor-x ed) 0) 
                           ed
                           (make-editor (editor-str ed) (- (editor-x ed) 1)))]
    [(string=? ke "right")
     (if (>= (editor-x ed) (string-length (editor-str ed)))
            ed
            (make-editor (editor-str ed) (+ (editor-x ed) 1)))]
    [else ed]))

(define (run ed)
  (big-bang ed
    [to-draw render]
    [on-key edit]
    ))
(run Empty)
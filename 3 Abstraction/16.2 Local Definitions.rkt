;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |16.2 Local Definitions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Addr] -> String 
; creates a string of first names, 
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing.v2 l)
  (local (; 1. extract names 
          (define names  (map address-first-name l))
          ; 2. sort the names 
          (define sorted (sort names string<?))
          ; 3. append them, add spaces 
          ; String String -> String
          ; appends two strings, prefix with " " 
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))

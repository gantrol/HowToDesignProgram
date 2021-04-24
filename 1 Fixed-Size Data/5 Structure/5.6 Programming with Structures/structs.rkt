;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname structs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])

(define-struct person [name hair eyes phone])

(define-struct pet [name number])

(define-struct CD [artist title price])

(define-struct sweater [material size producer])

(define-struct entry [name phone email])

(define-struct ball [location velocity])

(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")

(define-struct centry [name home office cell])
 
(define-struct phone [area number])
 
(make-centry "Shriram Fisler"
             (make-phone 207 "363-2421")
             (make-phone 101 "776-1099")
             (make-phone 208 "112-9981"))

(define-struct vel [deltax deltay])


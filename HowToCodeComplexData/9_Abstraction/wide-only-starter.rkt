;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wide-only-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; wide-only-starter.rkt

;
;PROBLEM:
;
;Use the built in version of filter to design a function called wide-only 
;that consumes a list of images and produces a list containing only those 
;images that are wider than they are tall.


(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 30 20 "solid" "red"))
(define I3 (rectangle 20 20 "solid" "red"))

;; (listof Image) -> (listof Image)
;; consumes a list of images and produces a list containing only those images that are wider than they are tall.
(check-expect (wide-only (list I1 I2 I3)) (list I2))
(check-expect (wide-only (list I1 I3)) empty)

;(define (wide-only loi) empty)

(define (wide-only loi)
  (local [(define (wider? i) (> (image-width i) (image-height i)))]
    (filter wider? loi)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; countdown-animation starter.rkt


;PROBLEM:
;
;Design an animation of a simple countdown. 
;
;Your program should display a simple countdown, that starts at ten, and
;decreases by one each clock tick until it reaches zero, and stays there.
;
;To make your countdown progress at a reasonable speed, you can use the 
;rate option to on-tick. If you say, for example, 
;(on-tick advance-countdown 1) then big-bang will wait 1 second between 
;calls to advance-countdown.
;
;Remember to follow the HtDW recipe! Be sure to do a proper domain 
;analysis before starting to work on the code file.
;
;Once you are finished the simple version of the program, you can improve
;it by reseting the countdown to ten when you press the spacebar.


(require 2htdp/image)
(require 2htdp/universe)
;; Simple countdown ten to zero


;; =================
;; Constants:
(define HEIGHT 400)
(define WIDTH 400)
(define FONT-SIZE 64)
(define FONT-COLOR "black")
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

;; Countdown is Number
;; interp. represent the number
(define C1 10)
(define C2 0)

#;
(define (fn-for-countdown c)
  (... c))

;; template rules used:
;;  - atomic non-distinct: number



;; =================
;; Functions:


;; Countdown -> Countdown
;; start the world with (main 10)
;;
(define (main c)
  (big-bang c ; Countdown
    (on-tick advance-countdown 1) ; Countdown -> Countdown
    (to-draw render) ;Countdown -> Image
    (on-key handle-key))) ; Countdown KeyEvent -> Countdown


;; Countdown -> Countdown
;; produce the next countdown
(check-expect (advance-countdown 10) 9)
(check-expect (advance-countdown 0) 0)

;(define (advance-countdown c) 0)

(define (advance-countdown c)
  (if (= c 0)
      0
      (- c 1)))


;; Countdown -> Image
;; render countdown
(check-expect (render 0) (place-image (text (number->string 0) FONT-SIZE FONT-COLOR ) CTR-X CTR-Y MTS))
(check-expect (render 10) (place-image (text (number->string 10) FONT-SIZE FONT-COLOR ) CTR-X CTR-Y MTS))

;(define (render c) empty-image)

(define (render c)
  (place-image (text (number->string c) FONT-SIZE FONT-COLOR ) CTR-X CTR-Y MTS))



;; Countdown KeyEvent -> Countdown
;; reset countdown to ten when space key is pressed
(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key 0 " ") 10)
(check-expect (handle-key 0 "a") 0)

;(define (handle-key c) 0) 
(define (handle-key c ke)
  (cond [(key=? ke " ") 10]
        [else c])) 

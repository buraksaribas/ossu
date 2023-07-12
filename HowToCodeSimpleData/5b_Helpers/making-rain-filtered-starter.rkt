;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname making-rain-filtered-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

;
;PROBLEM:
;
;Design a simple interactive animation of rain falling down a screen. Wherever we click,
;a rain drop should be created and as time goes by it should fall. Over time the drops
;will reach the bottom of the screen and "fall off". You should filter these excess
;drops out of the world state - otherwise your program is continuing to tick and
;and draw them long after they are invisible.
;
;In your design pay particular attention to the helper rules. In our solution we use
;these rules to split out helpers:
;  - function composition
;  - reference
;  - knowledge domain shift
;  
;  
;NOTE: This is a fairly long problem.  While you should be getting more comfortable with 
;world problems there is still a fair amount of work to do here. Our solution has 9
;functions including main. If you find it is taking you too long then jump ahead to the
;next homework problem and finish this later.



;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
    (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
    (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
    (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
;; !!!
(check-expect (handle-mouse empty 3 12 "button-down")
              (cons (make-drop 3 12) empty))
(check-expect (handle-mouse empty 3 12 "drag")
              empty)

;(define (handle-mouse lod x y mevt) empty) ; stub


(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops LOD2) (cons (make-drop 10 21) (cons (make-drop 3 7) empty)))
(check-expect (next-drops (cons (make-drop 10 299) empty)) (cons (make-drop 10 300) empty))
(check-expect (next-drops (cons (make-drop 10 300) (cons (make-drop 3 7) empty))) (cons (make-drop 3 8) empty))

;(define (next-drops lod)empty) ; stub

(define (next-drops lod)
  (filter-drops (tick-drops lod)))


;; Drop ListOfDrop -> ListOfDrop
;; filters drop where out of screen
(check-expect (tick-drops (cons (make-drop 10 299) empty)) (cons (make-drop 10 300) empty))
(check-expect (tick-drops (cons (make-drop 10 300) (cons (make-drop 3 7) empty))) (cons (make-drop 10 301) (cons (make-drop 3 8) empty)))

;(define (tick-drops d lod) empty) ; stub

(define (tick-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (next-drop (first lod))
               (tick-drops (rest lod)))]))


;; Drop -> Drop
;; produce next drop
(check-expect (next-drop (make-drop 10 30)) (make-drop 10 31))

;(define (next-drop d) empty) ; stub

(define (next-drop d)
  (make-drop (drop-x d) (+ 1 (drop-y d))))




;; ListOfDrop -> ListOfDrop
;; filters drop where out of screen
(check-expect (filter-drops (cons (make-drop 10 299) empty)) (cons (make-drop 10 299) empty))
(check-expect (filter-drops (cons (make-drop 10 301) (cons (make-drop 3 7) empty))) (cons (make-drop 3 7) empty))

;(define (filter lod) empty) ; stub


(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (in-screen? (first lod))
             (cons (first lod) (filter-drops (rest lod)))
             (filter-drops (rest lod)))]))



;; Drop -> Boolean
;; checks drop if its in screen returns true
(check-expect (in-screen? (make-drop 10 30)) true)
(check-expect (in-screen? (make-drop 10 301)) false)

;(define (in-screen? d) empty) ; stub

(define (in-screen? d)
  (<= (drop-y d) HEIGHT))




;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops (cons (make-drop 10 299) empty)) (place-image DROP 10 299 MTS))

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-drop (first lod)
                     (render-drops (rest lod)))]))

         
;; Drop Image -> Image
;; place drop on img as specified by d
(check-expect (place-drop (make-drop 9 5) MTS)
              (place-image DROP 9 5 MTS))

;(define (place-drop d) empty) ; stub

(define (place-drop d img)
  (place-image DROP (drop-x d) (drop-y d) img))

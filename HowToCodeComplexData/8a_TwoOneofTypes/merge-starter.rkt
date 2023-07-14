;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; merge-starter.rkt

;Problem:
;
;Design the function merge. It consumes two lists of numbers, which it assumes are 
;each sorted in ascending order. It produces a single list of all the numbers, 
;also sorted in ascending order. 
;
;Your solution should explicitly show the cross product of type comments table, 
;filled in with the values in each case. Your final function should have a cond 
;with 3 cases. You can do this simplification using the cross product table by 
;recognizing that there are subtly equal answers. 
;
;Hint: Think carefully about the values of both lists. You might see a way to 
;change a cell content so that 2 cells have the same value.

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 (list 1 3 5))
(define LON2 (list 2 4 6))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) ...]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;CROSS PRODUCT OF TYPE COMMENTS TABLE  
;
;                                              LON1
;                                     empty        (cons Number ListOfNumber) 
;
;L   empty                            empty          LON1             -->> LON1
;O
;N  (cons Number ListOfNumber)        LON2        (if (less? (first lon1) (first lon2))
;2                                                     (cons (first lon1) ...
(check-expect (merge empty empty) empty)
(check-expect (merge empty LON2) LON2)
(check-expect (merge LON1 empty) LON1)
(check-expect (merge LON1 LON2) (list 1 2 3 4 5 6))


;; ListOfNumber ListOfNumber -> ListOfNumber
;; merge lists ascending order (assume list ascending order)

(define (merge lon1 lon2)
  (cond [(empty? lon2) lon1]
        [(empty? lon1) lon2]
        [else (if (< (first lon1) (first lon2))
                  (cons (first lon1) (merge (rest lon1) lon2))
                  (cons (first lon2) (merge lon1 (rest lon2))))]))
        

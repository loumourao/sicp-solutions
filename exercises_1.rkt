#|
Exercise 1.1
The following lines will contain both the commands and the outputs 
which I assume the interpreter would print if one were to execute them
|#

10
;10
(+ 5 3 4)
;12
(- 9 1)
;8
(/ 6 2)
;3
(+ (* 2 4) (- 4 6))
;6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;19
(= a b)
;#f
(if (and (> b a) (< b (* a b)))
    b
    a)
;4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;16
(+ 2 (if (> b a) b a))
;6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;16

;Exercise 1.2
(/ (+ 5 
      4 
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;Exercise 1.3
(define (exercise_1.3 a b c)
    (define (square_sum a b)
        (+ (* a a) (* b b)))

    (if (>= a b)
        (if (>= b c)
            (square_sum a b)
            (square_sum a c))
        (if (>= a c)
            (square_sum b a)
            (square_sum b c))))

#|
Exercise 1.4
Since the interpreter follows applicative-order evaluation,
arguments/parameters will first be evaluated before expanding subsequent
procedures in which they are referenced, thus allowing for 'b' to be evaluated 
for the predicate of the conditional statement, thereby resulting in the 'abs' operator 
as follows in a LISP inspired pseudo-code:
if (b < 0) -> ((-) a (-b)) = (- a (-b)) = a + b
if (b > 0) -> ((+) a (+b)) = (+ a (+b)) = a + b
|#

#|
Exercise 1.5
Applicative-order evaluation - the code would run endlessly due to the infinite recursive call to 'p'
as evaluated in the function call to 'test'
Normal-order evaluation - the output would be '0' since, as opposed to the previous evaluation
strategy, the parameters would only be evaluated when required - such doesn't happen with the 
recursive call to 'p' given that the predicate of the conditional statement evaluates to #t, hence
avoiding the 'else' clause where the call to 'p' would, in fact, expand
|#


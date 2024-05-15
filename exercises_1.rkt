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

#|
Exercise 1.6
Since the new version of 'if' is a function, and following LISP's evaluation order, resorting to this
new version would result in an infinite recursive call to the 'sqrt-iter' function - as it stands for
the 3rd parameter passed to the function. Such would not be the case if one would use LISP's built-in 
'if' statement - as it would evaluate the predicate without passing on to evaluating its correspomding 
clauses
|#


;Exercise 1.7
(define (sqrt x)
    (define (sqrt-iter guess x)
        (define (average x y)
            (/ (+ x y) 2))

        (define (improve guess x)
            (average guess (/ x guess)))

        (define (good-enough? guess x)
            (< (abs (- (* guess guess) x)) 0.001))

        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                        x)))
    (sqrt-iter 1.0 x))

;let us first try to apply this function to both small and large numbers, respectively

(sqrt 0.00000001337) ;outputs the utterly innacurate result of 0.03125014247393297
(sqrt 9284756651928) ;provides us with no output as it runs endlessly

#|
If we print the intermediate results of 'guess', 'improved guess', and 'good-enough?' procedure, we
observe the following:

- For small numbers, in the 'good-enough?' procedure, the result satisfies the defined threshold at an
early stage whilst still having ample space for improvement - this is due to the fact that, for small
numbers, a smaller threshold is required to guarantee a higher-order of accuracy

- For large numbers, 'guess' and 'improved guess' converge and hit a plateau, yet, in the 
'good-enough?' procedure, the result does not satisfy the defined threshold and never will - provided
the previously mentioned convergence

If we were to decrease the threshold to a smaller number, this would help mitigate the accuracy 
challenge for small numbers, however, it would only make matters worse for large numbers. As such, one
must follow a different strategy to optimize this procedure.

We will now implement the authors' suggestion as presented in the statement
|#

(define (new-sqrt x)
    (define (sqrt-iter guess x)
        (define (average x y)
            (/ (+ x y) 2))

        (define (improve guess x)
            (average guess (/ x guess)))

        (define (good-enough? previous-guess guess)
            (< (abs (/ (- guess previous-guess) guess)) 0.001))

        (if (good-enough? guess (improve guess x))
            guess
            (sqrt-iter (improve guess x) 
                        x)))
    (sqrt-iter 1.0 x))

;let us now test this new implementation with the previous values

(new-sqrt 0.00000001337) ;outputs 0.00011562877669957844
(new-sqrt 9284756651928) ;outputs 3047190.4408145957

#|
Following these outputs, we can observe that this new strategy is much more accurate than the
previous one, even when using the same threshold. However, if we increase the threshold, it proves
even more accurate, as we can observe in the following code blocks
|#

(define (new-sqrt x)
    (define (sqrt-iter guess x)
        (define (average x y)
            (/ (+ x y) 2))

        (define (improve guess x)
            (average guess (/ x guess)))

        (define (good-enough? previous-guess guess)
            (< (abs (/ (- guess previous-guess) guess)) 0.00000001))

        (if (good-enough? guess (improve guess x))
            guess
            (sqrt-iter (improve guess x) 
                        x)))
    (sqrt-iter 1.0 x))

(new-sqrt 0.00000001337) ;outputs 0.00011562871615651393
(new-sqrt 9284756651928) ;outputs 3047089.8677333007

;Exercise 1.8
(define (cube_root x)
    (define (cube_root_iter guess x)
        (define (improve guess x)
            (/ (+ (/ x 
                      (* guess guess)) 
                  (* 2 guess)) 
                3))
        
        (define (good-enough? previous-guess guess)
            (< (abs (/ (- guess previous-guess) guess)) 0.00000001))
        
        (if (good-enough? guess (improve guess x))
            guess
            (cube_root_iter (improve guess x) 
                        x)))

    (cube_root_iter 1.0 x))

(cube_root 8)
(cube_root 125)
(cube_root 0.00000001337)
(cube_root 9284756651928)
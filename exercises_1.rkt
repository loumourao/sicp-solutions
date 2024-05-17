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
(define (exercise_3 a b c)
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

#|
Exercise 1.9
Let us illustrate the first and second process following the substitution model for (+ 4 5)
First process:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

Second process:
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

Procedurally/functionally speaking, both these procedures are recursive - has both bodies 
include a call to their corresponding definition. However, the same can't be said about the process 
that they generate. As we can observe through the illustrations, the first process is recursive, 
whilst the second one is iterative - this has to do with the fact that, setting aside time complexity 
(which is linear for both processes), if one were to focus on space complexity, the first 
process is linear and the second is constant, thus making the first process recursive and the second
process iterative; in other words, the first process plays a much greater impact in memory than the 
second process.
|#

#|
Exercise 1.10
Following the substitution model, we can see that (A 1 10) translates into a recursive process that 
fully expands in the following manner (A 0 (A 1 9)) -> (A 0 (A 0 (A 1 8))) -> ... -> (A 0 (A 0 (... (A 1 1)))). 
This is composed of 10 calls (counting with (A 1 1)). Since (A 1 1) outputs 2, we are now left with 
(A 0 (A 0 (... (A 0 2)))); now, whichever call whose first parameter equals 0, the output corresponds to the double of 
the second parameter, thus (A 0 2) = 4. Fully contracting this expansion, we can see that this follows
a pattern of 2^n, with n = 10. As such, the result is 2^10 = 1024.

For the second call, we can observe the same process, but with a slight difference. The difference lies
in the following case - instead of starting with (A 1 x), we now start with (A 2 4), which expands
differently in the first few steps: (A 1 (A 2 3)) -> (A 1 (A 1 (A 2 2))) -> (A 1 (A 1 (A 1 (A 2 1)))) ->
(A 1 (A 1 (A 1 (A 1 2)))) -> (A 1 (A 1 (A 0 2))) -> ... -> (A 1 (A 1 4)) -> following the same process
as shown above until it fully contracts. What we can observe here is that this follows the same pattern
as above, but performed in a "batch-wise" manner; that is, we first expand and reduce 2^2, resulting in 
(A 1 (A 1 4)), followed by a new expansion and reduction that makes use of the previous batch of operations, 
resulting in 2^4, followed by (A 1 (A 0 8)) -> (A 1 16), thus culminating, once again,
in 2^n calls, with n = 16. As such, the result is 2^16 = 65535

For the third call, such is not as trivial as the previous examples, and, for the sake of simplicity,
we will refrain from presenting a manual expansion and reduction and present only the final result. 

To summarize, we obtain the following answers:
(A 1 10) = 2^10    = 1024
(A 2 4)  = 2^(2^(2^2)) = 2^16 = 65535
(A 3 3)  = 65535

We will now confirm these observations by defining this procedure followed by corresponding calls 
comprised of the aforementioned parameters
|#

(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                   (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

(A 4 2)

#|
Considering the new expressions:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

And applying the processes which have previously been deconstructed, we are now capable of translating 
these procedures into compact mathematical notations, as follows:
f(n) = 2 * n
g(n) = 2^n
h(n) = 2^(h(n-1))
k(n) = 5 * (n^2)

P.S.: For those who are curious, the Ackermann function has little to no practical use and is often 
found within theoretical studies concerning computational complexity due to its rapid growth! 
(e.g., used as benchmark for a compiler's ability to optimize recursive processes)
|#

#|
Exercise 1.11
The following procedures will implement the intended function following recursive and iterative 
processes, respectively, together with corresponding tests
|#

(define (f_recursive n)
    (cond ((< n 3) n)
          (else (+ (f_recursive (- n 1))
                    (* 2 (f_recursive (- n 2)))
                    (* 3 (f_recursive (- n 3)))))))

(f_recursive 0)
(f_recursive 1)
(f_recursive 2)
(f_recursive 3)
(f_recursive 4)
(f_recursive 5)
(f_recursive 6)

(define (f_iterative n)
    (define (get_accumulation a b c)
        (+ a (* 2 b) (* 3 c)))

    (define (f_iterative_aux counter prior second_prior third_prior)
        (cond ((< n 3) n)
              ((= counter n) (get_accumulation prior second_prior third_prior))
              (else (f_iterative_aux (+ counter 1)
                                     (get_accumulation prior second_prior third_prior)
                                     prior
                                     second_prior
                                     ))))

    (f_iterative_aux 3 2 1 0))

(f_iterative 0)
(f_iterative 1)
(f_iterative 2)
(f_iterative 3)
(f_iterative 4)
(f_iterative 5)
(f_iterative 6)

#|
For a comparison measure regarding the efficiency of both implementations, we can observe the
impact that space complexity and, consequently, time complexity have in processing the following 
two calls pertaining to the iterative and recursive strategy, respectively. Whilst less user-friendly 
when code comprehension is concerned, it is obvious that the former is much more efficient than the 
latter. Furthermore, it is possible to further optimize the recursive process by resorting to 
techniques such as memoization. Memoization is a technique that is commonly applied in recursive 
scenarios where recursive trees expand mulitple sub-trees that have already been expanded before. It is 
applied in such a way that stores previously calculated values in an accessaible table - for lookup 
and fetch purposes - so as to prevent unnecessary recalculations, thus mitigating the recalculation of 
already expanded sub-trees and drastically diminishing the overhead related to these processes.
|#

(f_iterative 999)
(f_recursive 999)

;Exercise 1.12
(define (pascal_triangle n)
    (define (recursive_pascal row column)
        (cond ((or (= column 0)
                   (= column row)) 1)
              (else (+ (recursive_pascal (- row 1) (- column 1))
                       (recursive_pascal (- row 1) column)))))
    
    (define (print_pascal row_counter column_counter)
        (cond ((= row_counter n) (void))
              ((> column_counter row_counter) (display "\n")
                                              (print_pascal (+ row_counter 1) 0))
              (else (display (recursive_pascal row_counter column_counter))
                    (display " ")
                    (print_pascal row_counter (+ column_counter 1)))))
    
    (print_pascal 0 0))

(pascal_triangle 1)
(pascal_triangle 2)
(pascal_triangle 3)
(pascal_triangle 4)
(pascal_triangle 5)
(pascal_triangle 6)
(pascal_triangle 11)
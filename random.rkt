(module name racket



    (define (bingbing-add a b)
      (+ a b))


    (define (multi a b)
      (* a b))


    (define (square a)
      (* a a ))


    (define (sumofsquare a b)
      (+ (square a) (square b)))


    (define (bb-abs x)
      (cond ((>= x 0) x)
            ((< x 0) (- x))))


    (define (bb-abs-2 x)
      (cond ((>= x 0) x)
            (else (- x))))

    (define (formula a)
      (if (> a 20)
        (sumofsquare a 20)
        (abs a)))

    (define (weijj a)
      (if (and (> a 5) (< a 10))
        (sumofsquare a 10)
        a))


    ; 1.2
    (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
       (* 3 (- 6 2) (- 2 7)))

    ; 1.3
    (define (smaller a b)
      (if (> a b) b a))

    (define (smallest a b c)
      (smaller a (smaller b c)))

    (define (sum-of-bigger a b c)
      (cond ((= a (smallest a b c)) (sumofsquare b c))
            ((= b (smallest a b c)) (sumofsquare a c))
            (else (sumofsquare a b))))

    ; 1.4
    ; done
    (define (a-plus-abs-b a b) ((if (> b 0) + -) a b))

    ; sqrt
    (define (bb-sqrt n)
      (newg n 1.0))

    (define (newg n g)
      (define (good-enough? n g)
        (< (abs (- n (* g g))) 0.00001))

      (if (good-enough? n g)
        g
        (newg n (/ (+ (/ n g) g)
                   2))))


    ; 1.6
    ; What happens when Alyssa aempts to use this to compute square roots? Explain.
    ; answer: applicative verses normal order

    ; (define (new-if predicate then-clause else-clause)
    ;   (cond (predicate then-clause)
    ;         (else else-clause)))


    ; (define (sqrt-iter-new-if guess x)
    ;   (new-if (good-enough? guess x)
    ;           guess
    ;           (sqrt-iter (improve guess x) x)))

    ; (define (sqrt-iter guess x)
    ;   (if (good-enough? guess x)
    ;     guess
    ;     (sqrt-iter (improve guess x) x)))

    ; (define (improve guess x) (average guess (/ x guess)))

    ; (define (good-enough? guess x)
    ;   (< (abs (- (square guess) x)) 0.001))

    ; (define (average x y) (/ (+ x y) 2))

    ; 1.8

    (define (cube-root x)
      (cube-root-iter x 2.0))

    (define (cube-root-iter x y)

      (define (good-enough? y)
        (< (abs (- x (* y y y))) 0.00001))

      (define (improve y)
        (/ (+ (/ x (* y y)) (* 2 y)) 3))

      (if (good-enough? y)
        y
        (cube-root-iter x (improve y))))


    ; factorial
    ; n! = n * (n-1) * (n-2) * (n-3) * .... 1
    (define (factorial n)
      (if (= n 1)
        1
        (* n (factorial (- n 1)))))

    (define (factorial! n)
      (define (factorial-iter acc counter n)
        (if (> counter n)
          acc
          (factorial-iter (* acc counter) (+ 1 counter) n)))
      (factorial-iter 1 1 n))

    ; 1.10
    (define (A x y)
      (cond ((= y 0) 0)
            ((= x 0) (* 2 y))
            ((= y 1) 2)
            (else (A (- x 1) (A x (- y 1))))))

    ; (A 1 10) -> 1024
    ; (A 2 4) ->

    ; (A 3 3)
    ; (A 2 (A 3 2))
    ; (A 2 (A 2 (A 3 1)))
    ; (A 2 (A 2 2))
    ; (A 1 (A 2 (A 2 2)))
    ; (A 0 (A 1 (- (A 2 (A 2 2)) 1)))
    ; (* 2 (A 1 (- (A 2 (A 2 2)) 1)))

    (define (f n) (A 0 n)) ; -> 2 * n
    (define (g n) (A 1 n)) ; -> 2 **n
    (define (h n) (A 2 n)) ; -> 2 **(n * 2*x)
    (define (k n) (* 5 n n))


    (define (fib n) (fib-iter 1 0 n))
    (define (fib-iter a b count)
      (if (= count 0)
          b
          (fib-iter (+ a b) a (- count 1))))


    ; 1.11
    (define (f-1.11 n)
      (if (< n 3)
        n
        (+ (f-1.11 (- n 1))
           (* 2 (f-1.11 (- n 2)))
           (* 3 (f-1.11 (- n 3))))))

    ; stuck at coming up with the initial value, thought it was n n n
    (define (f-1.11-new n)
      (f-1.11-new-iter n 2 1 0))


    (define (f-1.11-new-iter count a b c)
      (if (< count 3)
        a
        (f-1.11-new-iter (- count 1)  (+ a (* 2 b) (* 3 c)) a b)))

    ; 1.12 use recursive
    (define (pascal row col)
      (cond ((= col 1) 1)
            ((= row 1) 1)
            ((= col row) 1)
            (else (+ (pascal (- row 1) (- col 1))
                     (pascal (- row 1) col)))))


    ; 1.15
    (define (sine x)

      (define (cube x) (* x x x))
      (define (p x) (- (* 3 x) (* 4 (cube x))))

      (if (<= (abs x) 0.1)
        x
        (p (sine (/ x 3.0)))))


     (define (cube x) (* x x x))
     (define (p x) (- (* 3 x) (* 4 (cube x))))
     (define (sine-sicp angle)
       (if (not (> (abs angle) 0.1))
         angle
         (p (sine-sicp (/ angle 3.0)))))


     (define (my-expt b n)
       (cond ((= n 0) 1)

             ((even? n) (* (my-expt b (/ n 2))
                           (my-expt b (/ n 2))))

             (else (* b (my-expt b (- n 1))))))



     ; (define (my-expt b n)
     ;   (cond ((= n 0) 1)
     ;         ((even? n) (square (my-expt b (/ n 2))))
     ;         (else (* b (my-expt b (- n 1))))))

     ; 1.16
     (define (fast-expt b n)
       (fast-expt-iter b n 1))

    (define (fast-expt-iter b count acc)
       (cond ((= count 1) acc)
             ((even? count) (fast-expt-iter (square b) (/ count 2) (square b)))
             (else (* b (fast-expt b (- count 1))))))

    (define (my-* a b)
      (my-iter-* a b 0))

    (define (my-iter-* a b sum)
      (cond ((= b 0) sum)
            (else (my-iter-* a (- b 1) (+ a sum)))))


    ; 1.17
    (define (mul a b)
      (mul-iter a b b))

    (define (double n) (* n 2))
    (define (halve n) (* 0.5 n))

    (define (mul-iter a b c)
      (cond ((or (= a 0) (= b 0)) 0)
            ((= a 1) c)
            ((even? a) (mul-iter (halve a) (double b) (+ c b)))
            (else (mul-iter (- a 1) b (+ c b)))))

    ; bb
    ; (define (bb-mul a b)
    ;   (bb-mul-iter a b a))

    ; (define (bb-mul-iter a b c)
    ;   (cond ((= b 0) 0)
    ;         ((= b 1) a)
    ;         ((even? b) (bb-mul-iter (double a) (halve b) c))
    ;         (else (bb-mul-iter (+ a c) (- b 1) c))))
    ; 1.2.5
    ; a->a+b
    ; b->a

    ; a+b+b
    ; a+b

    ; a+b+b+(a+b)
    ; a+b+b

    ; (define (fib n) (fib-iter 1 0 0 1 n))

    ; (define (fib-iter a b p q count)
    ;   (cond ((= count 0) b)
    ;         ((even? count)
    ;          (fib-iter a
    ;                    b

    ;                    ⟨??⟩ ; compute p′
    ;                    ⟨??⟩ ; compute q′

    ;                    (/ count 2)))
    ;         (else

    ;           ; how they came up with this formula!
    ;           (fib-iter (+ (* b q) (* a q) (* a p))
    ;                     (+ (* b p) (* a q))
    ;                     p
    ;                     q
    ;                     (- count 1)))))

    (define (gcd a b)
      (cond ((= b 0) a)
            (else (gcd (b (remainder a b))))))

    (define (smallest-divisor n)
      (find-divisor n 2))

    (define (find-divisor n divisor)
      ; (cond ((= n divisor) n)
      (cond ((> (square divisor) n) n)
            ((divines? n divisor) divisor)
            (else (find-divisor n (+ 1 divisor)))))

    (define (divines? a b) (= (remainder a b) 0))

    (define (prime? n)

      (define (pick-random n)
        (+ 1 (random (- n 1))))

      (define (prime-iter n a count)
        (define (prime-good-enough? count)
          (>= count 5))
        (cond ((prime-good-enough? count) #t)
              ((= (remainder (fast-expt a n) n) a)
               (prime-iter n (pick-random n) (+ 1 count)))
              (else #f)))

      (prime-iter n (pick-random n) 1))


    ; can't use runtime for some reason
    ; (define (test-prime-and-record-time n)
    ;   (display-run-time (runtime)))

    ; (define (display-run-time start-time)
    ;   (prime? n)
    ;   (display (- (runtime) start-time)))

    ; 1.23

    ; (smallest-divisor-new 1999999999999999999)

    (define (smallest-divisor-new n)
        (define (find-divisor-new n divisor)
          ; (cond ((= n divisor) n)

          (define (next num)
            (cond ((= num 2) 3)
                  (else (+ 2 num))))

          (cond ((> (square divisor) n) n)
                ((divines? n divisor) divisor)
                (else (find-divisor n (next divisor)))))

      (find-divisor-new n 2))


    ; (define (sum-integers a b)
    ;   (if (> a b)
    ;     0
    ;     (+ a (sum-integers (+ a 1) b))))


    (define (sum-integers a b)
      (define (next-a a) (+ a 1))
      (define (id-a a) a)
      (sum-2 id-a a next-a b))
      ; (sum- id-a a next-a b))


    (define (sum-cubes a b)
      (define (next-a a) (+ a 1))
      (sum- cube a next-a b))


    (define (pi-sum a b)
      (define (f-a a) (/ 1.0 (* a (+ a 2))))
      (define (add-4 a) (+ a 4))
      (sum- f-a a add-4 b))


    (define (sum- term a next b)
      (if (> a b)
        0
        (+ (term a)
           (sum- term (next a) next b))))

    ; 1.30
    (define (sum-2 term a next b)
      (define (iter a result)

        (if (> a b)
          result
          (iter (next a) (+ (term a) result))))

      (iter a 0))


    ; (define (sum- term a next b)
    ;   (if (> a b)
    ;     0
    ;     (+ (term a)
    ;        (sum- term (next a) next b))))

    (define (product term a next)
      (if (<= a 1)
        1
        (* (term a)
           (product term (next a) next))))

    (define (fact n)
      (define (identity x) x)
      (define (minus-one x) (- x 1))
      (product identity n minus-one))


    (define (fg g) (g 2))


    (define (fixed-point f first-guess)

      (define tolerance 0.00001)
      (define (good-enough? a b) (< (abs (- a b)) tolerance))

      (define (try-guess guess)
        (let ((next-guess (f guess)))
          (newline)
          (display next-guess)
          (cond ((good-enough? guess next-guess) next-guess)
                (else (try-guess next-guess)))))

      (try-guess first-guess))


    (define (average a b)
      (/ (+ a b)
         2))

    (define (avg-damp f)
      (lambda (x) (average x (f x))))


    ; y = sin y + cos y
    (define y (fixed-point (lambda (y) (+ (sin y) (sin y))) 5.0))

    ; y**2 = x
    ; y = x / y
    ; (define (sqrt-new x)

    ;   (fixed-point (lambda (y) (/ x y)) 1.0))
    (define (sqrt-new x)
      (fixed-point
        (lambda (y)
          (* (/ 1 2) (+ (/ x y) y)))
        1.0))


    ; 1.35 golden ratio
    ; x**2 = x + 1
    ; x = 1 + 1/x
    ; average ( x,  1+1/x)
    (define golden-ratio
      (fixed-point
        (lambda (x)
          (average x (+ 1 (/ 1 x))))
        1.0))

    ;1.36
    ; x**x = 1000

    (define athing
      (fixed-point
        (lambda (x) (average x (/ (log 1000) (log x))))
      2.0))


    ; 1.37
    (define (cont-frac-recur n d k)
      (define (cont-frac-recur-helper n d i k)
        (let ((ni (n i))
              (di (d i)))
          (cond ((= i k) (/ ni di))
                (else
                  (/ ni (+ di (cont-frac-recur-helper n d (+ i 1) k)))))))
      (cont-frac-recur-helper n d 1 k))


    ; accumulate from where the result could be computed, in this case, it's reverse from the back
    (define (cont-frac-iter n d k)
      (define (cont-frac-iter-helper n d k acc)
        (cond ((= k 0) acc)
              (else (cont-frac-iter-helper n d (- k 1) (/ (n k) (+ (d k) acc))))))
      (cont-frac-iter-helper n d k 0))

    ; 1.38

    (define de-frac-continuis
      (cont-frac-iter
        (lambda (i) 1.0)
        (lambda (i)
          (cond ((= (remainder i 3) 2) (* 2 (/ (+ 1 i) 3)))
                (else 1)))
        10))

    (define (tan-cf x k)
      (cont-frac-iter
        (lambda (i)
          (cond ((= i 1) x)
                (else (square x))))
        (lambda (i)
          (+ 1 (* 2 (- i 1))))
        k))

    (define (deriv g)
      (let ((dx 0.00001))
          (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

    ((deriv cube) 5)

    (define (newton g)
      (lambda (x)
        (- x (/ (g x) ((deriv g) x)))))

    (define (newtons-method g guess)
      (fixed-point (newton g) guess))

    (define (sqrt-ng x)
      (fixed-point
        (newton
          (lambda (y) (- (square y) x)))
        1.0))

    (define (fixed-point-formula g formula guess)
      (fixed-point (formula g) guess))

    ; y = x/y
    ; avg(y + x/y)

    (define (sqrt-1st-method x)
      (fixed-point-formula (lambda (y) (/ x y)) avg-damp 1.0))

    (define (sqrt-2nd-method x)
      (fixed-point-formula (lambda (y) (- (square y) x)) newton 1.0))

    ; 1.40
    ; (newtons-method (cubic a b c) 1)
    (define (cubic-formular x a b c)
      (define (cubic a b c)
        (lambda (y)
          ; x*x*x + a*x*x + b *x + c
          (- y
             (+ (* x x x)
                (* a (square x))
                (* b x)
                c))))

      (newtons-method (cubic a b c) 1))



    ; (define (sqrt-newton x)
    ;   (newtons-method
    ;       ; y*y = x
    ;       (lambda (y)
    ;         (- (square y)
    ;            x))
    ;       1.0))



)

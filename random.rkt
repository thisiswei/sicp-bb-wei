(module name racket
    ; have to put those in the xrepl
    ; (require xrepl)	

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

    ; 1.41
    (define (double-f f)
      (lambda (x) (f (f x))))

    ; 1.42
    (define (compose f g)
      (lambda (x) (f (g x))))

    (define (inc x)
      (+ x 1))

    ; 1.43
    (define (repeated f n)
      (cond ((= n 1) f)
            (else (compose f (repeated f (- n 1))))))

    (define (repeated-bb g n)
      (cond ((= n 1) g)
            (else (repeated-bb (compose g g) (- n 1)))))

    ; 1.44
    (define (smooth f)
      (define dx 0.00001)

      (lambda (x)
        (average
          (+ (f (- x dx))
             (f x))
          (f (+ x dx)))))

    (define (n-fold-smooth f n)
      (repeated (smooth f) n))

    ; 1.45
    (define (calculate-a n)
      (if (< n 2) 0
        (+ 1 (calculate-a (/ n 2)))))

    (define (nth-root n x)
      (let ((num-repeat (calculate-a n)))
        (fixed-point
          (repeated-bb
            ; figure out following by using simple example:
            ; ((repeated (avg-damp inc) 2) 2)
            (avg-damp (lambda (y) (/ x (my-expt y (- n 1)))))
            num-repeat)
          1.0)))

    (define (cubic-fixed-point x)
      (fixed-point
        (avg-damp
          (lambda (y)
            (/ x (square y))))
        1.0))

    (define (fourth-fixed-point x)
      (fixed-point
        (avg-damp
            (avg-damp
              (lambda (y)
                (/ x (* y y y)))))
        1.0))


    (define (eighth-fixed-point x)
      (fixed-point
        (avg-damp
            (avg-damp
                (avg-damp
                  (lambda (y)
                    (/ x (* y y y y y y y))))))
        1.0))

    ; 1.46
    (define (iterative-improve good-enough improve)
      (define (keep-improve guess)
        (cond ((good-enough guess) guess)
              (else (keep-improve (improve guess)))))
      keep-improve)

    (define (sqrt-iter-improve x)
      ((iterative-improve
         (lambda (y) (< (abs (- (* y y) x)) 0.001))
         (lambda (y) (average y (/ x y)))
         ) 1.0))

    (define (fixed-point-iter-improve f guess)
      ((iterative-improve
         (lambda (guess)
           (< (abs (- guess (f guess)))
              0.00001))
          f)
       guess))

    (define (test-fixed-point-iter x)
      (fixed-point-iter-improve (lambda (y) (/ x y)) 1.0))

    ; 2017-02-14
    ; 2.2
    (define (make-segment p1 p2)
      (cons p1 p2))


    (define (start-segment s)
      (car s))


    (define (end-segment s)
      (cdr s))


    (define (make-point x y)
      (cons x y))


    (define (x-point p)
      (car p))


    (define (y-point p)
      (cdr p))


    (define (print-point p)
      (newline)
      (display "(")
      (display (x-point p))
      (display ",")
      (display (y-point p))
      (display ")"))

    (define (midpoint-segment s)
      (let
        ((starts (start-segment s))
         (ends (end-segment s)))
        (make-point
          (average
            (x-point starts)
            (x-point ends))
          (average
            (y-point starts)
            (y-point ends)))))

    ; (print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 5 4))))
    ; (3,3)

    ; 2.3
    ; (define (make-rect s1 s2)
    ;   (cons s1 s2))


    ; (define (len rect)

    ;   (let ((s1 (car rect))
    ;         (s2 (cdr rect))

    ;         (s1-start (start-segment s1))
    ;         (s1-end (end-segment s1))

    ;         (s2-start (start-segment s2))
    ;         (s2-end (end-segment s2)))

    ;     (x-point start-s1)
    ;     (y-point end-s1)



    ;   )


    ; (define (wid rect)
    ;   (...))


    ; (define (perimeter rect)
    ;   (let ((l (len rect))
    ;         (w (wid rect)))
    ;     (* 2 (+ l w))))


    ; (define (area rect)
    ;   (let ((l (len rect))
    ;         (w (wid rect)))
    ;     (* l w)))

    ; 2.5
    (define (my-cons x y)
      (define (dispatch m)
        (cond ((= m 0) x)
              ((= m 1) y)
              (else (error "wtf"))))
      dispatch)

    (define (my-car z) (z 0))

    (define (my-cdr z) (z 1))


    (define (cons-2 x y)
      (lambda (m) (m x y)))

    (define (car-2 z)
      (z (lambda (p q) p)))

    (define (cdr-2 z)
      (z (lambda (p q) q)))


    (define (cons-3 a b)
      (* (my-expt 2 a)
         (my-expt 3 b)))

    (define (car-3 z)
      (cond ((odd? z) 0)
            (else (+ 1 (car-3 (/ z 2))))))

    (define (cdr-3 z)
      (cond
        ((= z 1) 0)
        ((even? z) (cdr-3 (/ z 2)))
        (else (+ 1 (cdr-3 (/ z 3))))))


    ; 2.6
    (define zero (lambda (f) (lambda (x) x)))

    (define (add-1 n)
      (lambda (f)
        (lambda (x) (f ((n f) x)))))

    (define one (lambda (f) (lambda (x) (f x))))

    (define two (lambda (f) (lambda (x) (f (f x)))))

    (define (plus a b)
      (lambda (f)
        (lambda (x)
          ((b f) ((a f) x)))))

    ; (plus two one)

    ; (f (f (f ..)))
    (define (my-append list1 list2)
      (if (null? list1)
        list2
        (cons (car list1)
              (my-append (cdr list1)
                         list2))))

    ; 2.17
    (define (last-pair items)
      (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))))

    ; 2.18
    (define (my-reverse items)
      (if (null? (cdr items))

        (cons (car items) null)
        (my-append
          (my-reverse (cdr items))
          (cons (car items) null))))

    (define (bb-reverse items)
      (if (null? items)
        null
        (my-append
          (bb-reverse (cdr items))
          (cons (car items) null))))



    ;     (define (bb-reverse list1)
    ;       (if (null? list1)
    ;         list1
    ;         (cons
    ;           (bb-reverse (cdr list1))
    ;                       (car list1))))

    ; 2.19
    (define (cc amt coin-values)
      (cond
        ((= amt 0) 1)
        ((or (< amt 0) (no-more? coin-values)) 0)
        (else
          (+ (cc
               (- amt (first-denominator coin-values))
               coin-values)
             (cc
               amt
               (rest-coin-values coin-values))))))

    (define (no-more? coin-values)
      (null? coin-values))

    (define (first-denominator coin-values)
      (car coin-values))

    (define (rest-coin-values coin-values)
      (cdr coin-values))

    ; 2.20
    (define (same-parity n . z)
      (let
        ((p (if (odd? n)
             odd?
             even?)))
        (same-parity-helper p (list n) z)))

    (define (same-parity-helper p acc z)
      (cond ((null? z) acc)
            ((p (car z)) (same-parity-helper
                           p
                           (append acc (cons (car z) null))
                           (cdr z)))
            (else (same-parity-helper p acc (cdr z)))))

    (define (same-parity-bb x . z)
      (if (even? x)
        (append (list x) (filter even? z))
        (append (list x) (filter odd? z))))

    ; 2.21
    (define (square-list items)
      (if (null? items)
        null
        (cons (square (car items))
              (square-list (cdr items)))))

    (define (square-list-2 items)
      (map square items))

    ; 2.23
    (define (for-each p items)
      (cond ((null? items) #t)
            ; ((and (p (car items)) #t) (for-each p (cdr items)))))
            (else (p (car items))
                  (for-each p (cdr items)))))

    (for-each (lambda (x)
                (newline)
                (display x))
              (list 57 321 88))

    (define (count-leaves t)
      (cond ((null? t) 0)
            ((leaf? t) 1)
            (+ (count-leaves (car t))
               (count-leaves (cdr t)))))

    (define (leaf? t)
      (not (pair? t)))


    ; 2.25
    ; (1 3 (5 7) 9)
    ; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

    ; ((7))
    ; (car (car (cons (cons 7 null) null)))

    ; (car (cdr    (cdr (cdr (cdr (cdr (cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 null))))))))))))))
    ; (1 (2 (3 (4 (5 (6 7))))))
    (define my-x (list 1 2 3))
    (define my-y (list 4 5 6))
    (append my-x my-y)
    (cons my-x my-y)
    (list my-x my-y)

    ; 2.27
    (define (deep-reverse x)
      (cond ((null? x) null)
            ((pair? x)
             (append

               (deep-reverse
                 (cdr x))

               (list (deep-reverse (car x)))))

            (else x)))

    ; 2.28
    (define (fringe x)
      (cond ((null? x) null)
            ((pair? x)
             (append (fringe (car x))
                     (fringe (cdr x))))
            (else (list x))))

    ; 2.29
    (define (make-mobile left right)
      (list left right))

    (define (make-branch real-length structure)
      (list real-length structure))

    ; a 1
    (define (left-branch m)
      (car m))

    ; a 1
    (define (right-branch m)
      (car (cdr m)))

    ; a 2
    (define (branch-length b)
      (car b))

    ; a 2
    (define (branch-structure b)
      (car (cdr b)))

    ; b 1
    (define (total-weight m)
      (+ (total-weight-branch (left-branch m))
         (total-weight-branch (right-branch m))))

    (define (total-weight-branch b)
      (cond ((is-leaf? b) (branch-structure b))
            (else (total-weight (branch-structure b)))))

    (define (is-leaf? b)
      (not (pair? (branch-structure b))))

    ; (total-weight (make-mobile (make-branch 1 3) (make-branch 1 3)))

    ; (total-weight (make-mobile (make-branch 1 3) (make-branch 1 (make-mobile (make-branch 1 3) (make-branch 1 3)))))
    ; 9

    (define (total-weight-bb m)
      (tw-helper m 0))

    (define (tw-helper m init)
      (cond ((null? m) init)
            ((pair? m) (+ (tw-helper (branch-structure (left-branch m)) init)
                          (tw-helper (branch-structure (right-branch m)) init)))
            (else m)))

    ; 2.29
    (define (balanced? m)
      (let ((lf (left-branch m))
            (rt (right-branch m)))
        (and (balanced-branch? lf)
             (balanced-branch? rt)
             (= (weight-of lf) (weight-of rt)))))

    (define (balanced-branch? b)
      (let ((s (branch-structure b)))
        (cond ((is-mobile? s) (balanced? s))
              (else #t))))

    (define (weight-of b)
      (let ((s (branch-structure b)))
        (cond ((is-mobile? s) (total-weight s))
              (else (* (branch-length b) s)))))

    (define (is-mobile? s)
      (pair? s))

    ; bb
    (define (balanced-bb? m)
      (and (b-balanced? (car m))
           (b-balanced? (car (cdr m)))
           (= (balanceWeight (car m))
              (balanceWeight (car (cdr m))))))

    (define (b-balanced? b)
      (if (pair? (car (cdr b)))
        (balanced-bb? (car (cdr b)))
        #t))

    (define (balanceWeight b)
      (if (pair? (car (cdr b)))
        (total-weight (car (cdr b)))
        (* (car b) (car (cdr b)))))


    (define (scale-tree tree factor)
      (cond ((null? tree) null)
            ((not (pair? tree)) (* tree factor))
            (else (cons (scale-tree (car tree) factor)
                        (scale-tree (cdr tree) factor)))))

    (define (scale-tree-map tree factor)
      (map (lambda (x)
             (cond ((pair? x) (scale-tree-map x factor))
                   (else (* x factor)))) tree))

    (scale-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

    ; 2.30

    (define (square-tree tree)
      (cond ((null? tree) null)
            ((not (pair? tree)) (square tree))
            (else (cons (square-tree (car tree))
                        (square-tree (cdr tree))))))


    (define (square-tree-map tree)
      (map
        (lambda (sub-tree)
          (cond ((not (pair? sub-tree)) (square sub-tree))
                (else (square-tree-map sub-tree))))
        tree))


    ; (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

    ; 2.31
    (define (tree-map f tree)
      (cond ((null? tree) null)
            ((not (pair? tree)) (f tree))
            (else
              (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

    (define (square-tree-abstracted tree)
      (tree-map square tree))


    ; 2.32
    (define (subset tree)
      (if (null? tree)
        (list tree)
        (let ((head (car tree))
              (t-rest (cdr tree)))
          (append
            (subset t-rest)
            (map (lambda (x)
                   (cons head x))
                 (subset t-rest))))))

    (define (enumerate-interval low high)
      (if (> low high) null
        (cons low (enumerate-interval (+ 1 low) high))))

    (define (enumerate-tree tree)
      (cond ((null? tree) null)
            ((pair? tree) (append (enumerate-tree (car tree))
                                  (enumerate-tree (cdr tree))))
            (else (list tree))))

    (define (my-accumulate-fold-left op initial sequence)
      (cond ((null? sequence) initial)
            (else
              (my-accumulate-fold-left
                op

                (op initial (car sequence))

                (cdr sequence)))))

    (define (my-accumulate-fold-right op initial sequence)
      (if (null? sequence) initial
        (op (car sequence)
            (my-accumulate-fold-right op initial (cdr sequence)))))

    (define (sum-odd-squares tree)
      (my-accumulate-fold-left
        +
        0
        (map square
             (filter odd? (enumerate-tree tree)))))

    ; (define (even-fibs n)
    ;   (filter
    ;     even?
    ;     (map
    ;       fibs
    ;       (enumerate-interval 0 n))))

    ; (define (salary-of-highest-paid-programmer records)
    ;   (max
    ;     (map salary (filter programmer? records))))

    ; 2.33
    (define (my-map p sequence)
      (my-accumulate-fold-left
        (lambda (y x)
          (append y (list (p x))))
        null
        sequence))

    (define (my-append-2 seq1 seq2)
      (my-accumulate-fold-left
        cons seq2 seq1))

    (define (my-length sequence)
      (my-accumulate-fold-left
        (lambda (x y)
          (+ 1 x))
        0
        sequence))

    ; 2.34
    (define (horner-eval-1 x coe-seq)
      (my-accumulate-fold-left
        (lambda (this-coe higher-terms)
          (+ (* higher-terms x)
             this-coe))
          0
          (reverse coe-seq)))

    (define (horner-eval-2 x coe-seq)
      (my-accumulate-fold-right
        (lambda (this-coe higher-terms)
          (+ (* higher-terms x)
             this-coe))
          0
          coe-seq))

    ; 2.35
    (define (count-leaves-2 t)
      (my-accumulate-fold-right
        +
        0
        (map
          (lambda (x) 1)
          (enumerate-tree t))))

    (define (count-leaves-bb t)
      (my-accumulate-fold-right
        +
        0
        (map
          (lambda (x)
            (if (pair? x)
              (count-leaves-bb x)
              1))
          t)))

    ; 2.36
    (define (accumulate-n op init seqs)
      (if (null? (car seqs))
        null
        (cons (my-accumulate-fold-right op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))
    ; (accumulate-n + 0 '('(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))

    (define (dot-product v w)
      (my-accumulate-fold-right + 0 (map * v w)))

    ; "random.rkt"> (map * '(1 2) '(2 4))
    ; '(2 8)

    ; 2.37
    (define (matrix-*-vector m v)
      (map
        (lambda (w)
          (dot-product v w))
        m))

    (define (transpose mat)
      (accumulate-n
        cons
        null
        mat))

    (define (matrix-*-matrix m n)
      (let ((cols (transpose n)))
        (map
          (lambda
            (v)
            (matrix-*-vector cols v))
            m)))

    ; 2.39
    (define (reverse-fold-right sequence)
      (my-accumulate-fold-right
        (lambda (x y)
          (append y (list x)))
        null
        sequence))

    (define (reverse-fold-left sequence)
      (my-accumulate-fold-left
        (lambda (x y)
          (append (list y) x))
        null
        sequence))

    (define (get-pair n)
      (my-accumulate-fold-left
        append
        null
        (map (lambda (i)
            (map (lambda (j)
                (list i j))
                  (enumerate-interval 1 (- i 1))))
              (enumerate-interval 1 n))))

    (define (flat-map proc sequence)
      (my-accumulate-fold-right
        append
        null
        (map proc sequence)))

    (define (permutations s)
      (if (null? s)
        (list null)
        (flat-map
          (lambda (x)
            (map
              (lambda (r) (cons x r))
              (permutations (remove x s))))
          s)))

    (define (my-remove s elem)
      (cond ((null? s) null)
            ((= (car s) elem) (cdr s))
            (else (cons (car s) (my-remove (cdr s) elem)))))

    ; 2.40
    (define (unique-pairs n)
      ; pairs (i, j)
      ; 1 <= j < i <= n
      (flat-map
        (lambda (i)
          (map
            (lambda (j)
              (list i j))
            (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))


    (define (unique-pairs-bb n)
      (map
        (lambda (i)
          (map
            (lambda (j)
              (list i j))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

    ; 2.41
    ; (i,j,k <=n)
    ; i+j+k == s
    (define (find-integer n s)
      (flat-map permutations
        (filter
          (lambda (p)
            (let ((p1 (car p))
                  (p2 (cadr p))
                  (p3 (caddr p)))
              (= (+ p1 p2 p3) s)))
          (find-pairs n))))

    (define (find-pairs n)
      (flat-map
        (lambda (i)
          (flat-map
            (lambda (j)
              (map
                (lambda (k) (list i j k))
                (enumerate-interval 1 (- j 1))))
              (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))


    ; 2.42
    (define (queens board-size)
      (define empty-board null)

      (define (queens-cols k)
        (cond ((= k 0) (list empty-board))
              (else
                (filter
                  (lambda (positions) (safe? k positions))
                  (flat-map
                    (lambda (rest-queens-positions)
                      (map (lambda (row)
                          (combine-queens row rest-queens-positions))
                        (enumerate-interval 1 board-size)))
                    (queens-cols (- k 1)))))))
      (queens-cols board-size))

    (define (safe? k positions)
      (define (safe-iterate n ps)
        (cond ((= k n) #t)
              ((null? ps) #t)
              (else
                (let ((current-position (car ps)))
                  (and (not (= current-position (last positions)))
                       (not (= (- k n) (abs (- (last positions) current-position))))
                       (safe-iterate (+ n 1) (cdr ps)))))))
      (safe-iterate 1 positions))

    (define (safe-bb? cols positions)
      (let ((last-p (last positions)))
        (define (loop? cols positions count)
          (cond ((= count cols) #t)
                ((= (car positions) last-p) #f)
                ((= (abs (- last-p (car positions))) (- cols count)) #f)
                (else (loop? cols (cdr positions) (+ count 1)))))
        (loop? cols positions 1))
        )

    (define (combine-queens row rest-queens-positions)
      (append rest-queens-positions (list row)))

    ; 2017-03-06
    (define (memq item xs)
      (cond ((null? xs) #f)
            ((eq? (car xs) item) xs)
            (else (memq item (cdr xs)))))

    ;2.53
    ; (list 'a 'b 'c) => '(a b c)
    ; (list (list 'george)) => '((george))
    ; (cdr '((x1 x2) (y1 y2))) => '((y1 y2))
    ; (cadr '((x1 x2) (y1 y2))) => '(y1 y2)
    ; (pair? (car '(a short list))) => #f
    ; (memq 'red '((red shoes) (blue socks))) => #f
    ; (memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

    ; 2.54
    (define (my-equal? seq1 seq2)
      (cond ((and (pair? seq1) (pair? seq2)) (and (my-equal? (car seq1) (car seq2))
                                                  (my-equal? (cdr seq1) (cdr seq2))))
            ((and (not (pair? seq1)) (not (pair? seq2))) (eq? seq1 seq2))
            ((and (null? seq1) (null? seq2)) #t)
            (else #f)))

    (define (bb-equal? list1 list2)
      (cond ((and (null? list1) (null? list2)) #t)
            ((and (pair? list1) (pair? list2)) (and (bb-equal? (car list1) (car list2))
                                                    (bb-equal? (cdr list1) (cdr list2))))
            ((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
            (else #f)))

    ; 2017-03-07
    ; deriv not working properly
    (define (deriv-2 f x)
      (cond ((number? f) 0)
            ((variable? f) (if (same-variable? f x) 1 0))
            ((sum? f) (make-sum
                        (deriv-2 (addend f) x)
                        (deriv-2 (augend f) x)))
            (product? f) (make-sum
                           (make-product
                             (multiplier f)
                             (deriv-2 (multiplicand f) x))
                           (make-product
                             (deriv-2 (multiplier f) x)
                             (multiplicand f)))))

    (define (variable? f) (symbol? f))
    (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
    (define (make-sum u v) (list '+ u v))
    (define (addend f) (cadr f))
    (define (augend f) (caddr f))

    (define (make-product u v) (list '* u v))
    (define (multiplier f) (cadr f))
    (define (multiplicand f) (caddr f))

    (define (sum? f) (and (pair? f) (eq? (car f) '+)))
    (define (product? f) (and (pair? f) (eq? (car f) '*)))

    ; official
    (define (deriv-3 exp var)
      (define (variable? x) (symbol? x))
      (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
      (define (make-sum a1 a2) (list '+ a1 a2))
      (define (make-product m1 m2) (list '* m1 m2))
      (define (sum? x) (and (pair? x) (eq? (car x) '+)))
      (define (addend s) (cadr s))
      (define (augend s) (caddr s))
      (define (product? x) (and (pair? x) (eq? (car x) '*)))
      (define (multiplier p) (cadr p))
      (define (multiplicand p) (caddr p))

      (cond ((number? exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            ((sum? exp) (make-sum-2 (deriv-3 (addend exp) var)
                                  (deriv-3 (augend exp) var)))
            ((product? exp)
             (make-sum-2
               (make-product-2 (multiplier exp)
                             (deriv-3 (multiplicand exp) var))
               (make-product-2 (deriv-3 (multiplier exp) var)
                             (multiplicand exp))))
            (else (error "error"))))

    (define (make-sum-2 a1 a2)
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1)
                  (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))

    (define (make-product-2 a1 a2)
      (cond ((=number? a1 0) 0)
            ((=number? a2 0) 0)
            ((=number? a2 1) a1)
            ((=number? a1 1) a2)
            ((and (number? a1)
                  (number? a2)) (* a1 a2))
            (else (list '* a1 a2))))

    (define (=number? exp x) (and (number? exp) (eq? x exp)))

    ; sets as unordered lists
    (define (element-of-set? x set)
      (cond ((null? set) #f)
            ((equal? x (car set)) #t)
            (else (element-of-set? x (cdr set)))))

    (define (adjoin-set x set)
      (if (element-of-set? x set)
        set
        (cons x set)))

    (define (intersection-set set1 set2)
      (define (intersection-set-iter set1 set2 res)
        (cond ((or (null? set1) (null? set2)) res)
              ((element-of-set? (car set1) set2) (intersection-set-iter (cdr set1) set2 (cons (car set1) res)))
              (else (intersection-set-iter (cdr set1) set2 res))))
      (intersection-set-iter set1 set2 null))

    (define (intersection-set-accumulate set1 set2)
      (my-accumulate-fold-right
        (lambda (s res)
          (cond ((element-of-set? s set1) (cons s res))
                (else res)))
        null
        set2))

    ; 2.59
    (define (union-set set1 set2)
      (my-accumulate-fold-right
        (lambda (s res)
          (adjoin-set s res))
        set1
        set2))

    (define (intersection-set-orderset set1 set2)
      (define (intersection-set-orderset-iter s1 s2 res)
        (if (or (null? s1) (null? s2)) res
          (let ((x1 (car s1))
                (x2 (car s2)))
            (cond
              ((= x1 x2) (intersection-set-orderset-iter (cdr s1) (cdr s2) (cons x1 res)))
              ((> x1 x2) (intersection-set-orderset-iter s1 (cdr s2) res))
              ((< x1 x2) (intersection-set-orderset-iter (cdr s1) s2 res))))))
      (intersection-set-orderset-iter set1 set2 null))

    (define (element-of-set-orderset? elem xs)
      (cond ((null? xs) #f)
            (else
              (let ((head (car xs))
                    (rest (cdr xs)))
                (cond ((= head elem) #t)
                      ((> head elem) #f)
                      ((< head elem) (element-of-set-orderset? elem rest)))))))

    ; 2.61
    (define (adjoin-set-orderset elem xs)
      (if (null? xs)
        (cons elem null)
        (let ((head (car xs))
              (rest (cdr xs)))
          (cond ((= head elem) xs)
                ((> head elem) (cons elem xs))
                ((< head elem) (cons
                                 head
                                 (adjoin-set-orderset elem rest)))))))

    ; 2.61 bb
    (define (ordered-adjoin x set)
      (cond ((null? set) (cons x set))
            ((= x (car set)) set)
            ((> x (car set)) (cons (car set)
                                   (ordered-adjoin x (cdr set))))
            ((< x (car set)) (cons x set))))

    ; 2.62
    (define (ordered-union-set set1 set2)
      (if (or (null? set1) (null? set2))
        (append set1 set2)
        (let
          ((head1 (car set1))
           (head2 (car set2))
           (rest1 (cdr set1))
           (rest2 (cdr set2)))
          (cond ((= head1 head2) (cons head1 (ordered-union-set rest1 rest2)))
                ((> head1 head2) (cons head2 (ordered-union-set set1 rest2)))
                ((< head1 head2) (cons head1 (ordered-union-set rest1 set2)))))))

    ; 2.62
    (define (od-union s1 s2)
      (cond ((null? s1) s2)
            ((null? s2) s1)
            ((> (car s1) (car s2)) (cons (car s2) (od-union s1 (cdr s2))))
            ((= (car s1) (car s2)) (cons (car s1) (od-union (cdr s1) (cdr s2))))
            ((< (car s1) (car s2)) (cons (car s1) (od-union s2 (cdr s1))))))

    (define (entry tree)
      (car tree))

    (define (tree-left-branch tree)
      (cadr tree))

    (define (tree-right-branch tree)
      (caddr tree))

    (define (make-tree entry left right)
      (list entry left right))

    (define (elem-of-set-tree? x set)
      (if (null? set)
        #f
        (let ((current (entry set)))
          (cond ((= current x) #t)
                ((< current x) (elem-of-set-tree? x (tree-right-branch set)))
                ((> current x) (elem-of-set-tree? x (tree-left-branch set)))))))

    ; bb
    (define (adj-tree x set)
      (cond ((null? set) (make-tree x null null))
            ((= x (entry set)) set)
            ((> x (entry set)) (make-tree
                                 (entry set)
                                 (tree-left-branch set)
                                 (adj-tree x (tree-right-branch set))))
            ((< x (entry set)) (make-tree
                                 (entry set)
                                 (adj-tree x (tree-left-branch set))
                                 (tree-right-branch set)))))



    (define (adjoin-set-tree x set)
      (if (null? set)
        (make-tree x null null)
        (let ((lb (tree-left-branch set))
              (rb (tree-right-branch set))
              (e (entry set)))
          (cond
            ((= x e) set)
            ((> x e) (make-tree e lb (adjoin-set-tree x rb)))
            ((< x e) (make-tree e (adjoin-set-tree x lb) rb))))))

    (define my-tree
      (make-tree 5
        (make-tree 3 (make-tree 1 null null) null)
        (make-tree 9 (make-tree 7 null null) (make-tree 11 null null))))


    ; (define (display-tree t)
    ;   (cond ((null t) (display ""))
    ;         (else ((lambda (x)
    ;                 (display-tree (tree-left-branch t))
    ;                 (display (entry t))
    ;                 (display-tree (tree-right-branch t))) t))))

    ; 2.63
    (define (tree->list-1 tree)
      (if (null? tree)
        null
        (append
          (tree->list-1 (tree-left-branch tree))
          (list (entry tree))
          (tree->list-1 (tree-right-branch tree)))))

    (define (tree->list-2 tree)
      (define (tree-list-iterate tree result-list)
        (cond ((null? tree) result-list)
              (else (tree-list-iterate
                      (tree-left-branch tree)
                      (cons (entry tree)
                            (tree-list-iterate
                              (tree-right-branch tree)
                              result-list))))))

      (tree-list-iterate tree null))
    ; 2.64
    ; converts an given list to a balanced tree
    ; (define (list->tree ls)

    ;   (define (pick-mid-point ls)
    ;     (let ((len (length ls)))
    ;       (cond ((odd? len) (/ (+ 1 len) 2))
    ;             ((even? len) (/ len 2)))))


      ; (cond ((null? ls) null)
      ;       ((null? (cdr ls)) (make-tree (car ls) null null))
      ;       (else
      ;         (let ((mid (pick-mid-point ls))
      ;               (left-half (pick-left-half ls))
      ;               (right-half (pick-right-half ls)))
      ;         (make-tree
      ;           mid
      ;           (list->tree left-half)
      ;           (list->tree right-half))))))


    ; 2.66
    (define (lookup-binary given-key tree)
      (define (key record) (car record))

      (cond ((null? tree) #f)
            ((> given-key (key (entry tree))) (lookup-binary given-key (tree-right-branch tree)))
            ((< given-key (key (entry tree))) (lookup-binary given-key (tree-right-branch tree)))
            ((= given-key (key (entry tree))) (entry tree))))


    ; representing huffman trees
    (define (make-leaf symbol weight)
      (list 'leaf symbol weight))

    (define (leaf-huffman? object) (eq? (car object) 'leaf))

    (define (symbol-leaf x) (cadr x))

    (define (weight-leaf x) (caddr x))

    (define (left-branch-huffman t)
      (car t))

    (define (right-branch-huffman t)
      (cadr t))

    (define (symbols t)
      (if (leaf-huffman? t)
        (list (symbol-leaf t))
        (caddr t)))

    (define (weight t)
      (if (leaf-huffman? t)
        (weight-leaf t)
        (cadddr t)))

    (define (make-code-tree left right)
      (list
        left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


    ; the base case should be simple!
    (define (decode bits tree)
      (define (decode-1 bs branch)
        (if (null? bs)
          null
          (let ((next-branch (choose-next-branch (car bs) branch)))
            (if (leaf-huffman? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bs) tree))
              (decode-1 (cdr bs) next-branch)))))
      (decode-1 bits tree))

    (define (choose-next-branch bit branch)
      (cond ((= bit 0) (left-branch-huffman branch))
            ((= bit 1) (right-branch-huffman branch))))


    ; wrong answer
    ; (define (decode bits tree)
    ;   (define (decode-inter bs t res)
    ;     (cond 
    ;       ((and (leaf-huffman? t) (null? bs)) (cons (symbol-leaf t) res))
    ;       ((leaf-huffman? t) (decode-inter (cdr bs) tree (cons (symbol-leaf t) res)))
    ;       ((or (null? bs) (null? t)) res)
    ;       ((= (car bs) 0) (decode-inter (cdr bs) (left-branch-huffman t) res))
    ;       ((= (car bs) 1) (decode-inter (cdr bs) (right-branch-huffman t) res))))
    ;   (decode-inter bits tree null))


    ; wrong answer
    ; (define (decode-bb bits tree) (decode-bb-helper bits tree null))
    ; (define (decode-bb-helper bits tree res)
    ;   (cond
    ;     ((and (leaf-huffman? tree) (null? bits)) (append res (list (symbol-leaf tree))))
    ;     ((leaf-huffman? tree) (append res (list (symbol-leaf tree)) (decode-bb (cdr bits) tree)))
    ;     ((= (car bits) 0) (decode-bb-helper (cdr bits) (car tree) res))
    ;     ((= (car bits) 1) (decode-bb-helper (cdr bits) (cadr tree) res))))

    ; wrong answer
    ; (define (decode-bb-helper bits tree res)
    ;   (if (leaf-huffman? tree)
    ;     (cond
    ;       ((null? bits) (append res (list (symbol-leaf tree))))
    ;       (else (append
    ;               res
    ;               (list (symbol-leaf tree))
    ;               (decode-bb (cdr bits) tree))))

    ;     (if (= (car bits) 0)
    ;       (decode-bb-helper
    ;         (cdr bits)
    ;         (car tree)
    ;         res)
    ;       (decode-bb-helper
    ;         (cdr bits)
    ;         (cadr tree)
    ;         res))))

    ; wrong answer
    ; (define (real-decode bits tree)
    ;   (decode bits tree null))

    ; (define (decode bits tree res)
    ;   (cond
    ;     ((and (null? bits) (leaf-huffman? tree)) (append res (list (symbol-leaf tree))))
    ;     ((null? bits) res)
    ;     ((leaf-huffman? tree) (append res (list (symbol-leaf tree)) (real-decode (cdr bits) tree)))

    ;     ((= (car bits) 1) (append (decode (cdr bits) (cadr tree) res)
    ;                                 (real-decode (cdr bits) tree)))

    ;     ((= (car bits) 0) (append (decode (cdr bits) (car tree) res)
    ;                                 (real-decode (cdr bits) tree)))))


    ; "random.rkt"> my-code-tree
    ; '((leaf A 8)
    ;   (((leaf B 3) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 5)
    ;    (((leaf E 1) (leaf F 1) (E F) 2)
    ;     ((leaf G 1) (leaf H 1) (G H) 2)
    ;     (E F G H)
    ;     4)
    ;    (B C D E F G H)
    ;    9)
    ;   (A B C D E F G H)
    ;   17)
    (define my-code-tree
      (make-code-tree
        ; A, 8
        (make-leaf 'A 8)

        ; BCDEFGH, 9
        (make-code-tree
          ; BCD, 5
          (make-code-tree
            ; left B3
            (make-leaf 'B 3)
            ; CD 2
            (make-code-tree
              ;C1
              (make-leaf 'C 1)
              (make-leaf 'D 1)))

          ; EFGH, 4
          (make-code-tree
            ; EF, 2
            (make-code-tree
              (make-leaf 'E 1)
              (make-leaf 'F 1))

            ; GH, 2
            (make-code-tree
              (make-leaf 'G 1)
              (make-leaf 'H 1))))))

    ; 2017-03-12
    ; 2.68
    (define (encode message tree)
      (if (null? message)
        null
        (append
          (encode-symbol (car message) tree)
          (encode (cdr message) tree))))

    (define (encode-symbol msg tree)
      (if (leaf-huffman? tree)
        null
        (let ((symbols-left (symbols (left-branch-huffman tree)))
              (symbols-right (symbols (right-branch-huffman tree))))
          (cond ((elem? msg symbols-left) (cons 0 (encode-symbol msg (left-branch-huffman tree))))
                ((elem? msg symbols-right) (cons 1 (encode-symbol msg (right-branch-huffman tree))))
                (else (error "found nothing"))))))

    (define (elem? x xs)
      (cond ((null? xs) #f)
            ((eq? (car xs) x) #t)
            (else (elem? x (cdr xs)))))

    (define (encode-bb message tree)
      (if (null? message)
        null
        (append
          (encode-symbol-bb (car message) tree)
          (encode-bb (cdr message) tree))))

    (define (encode-symbol-bb msg tree)
      (if (null? tree)
        null
        (let ((left (left-branch-huffman tree))
              (right (right-branch-huffman tree)))
          (cond ((leaf-huffman? tree) null)
                ((elem? msg (symbols left)) (cons 0 (encode-symbol-bb msg left)))
                ((elem? msg (symbols right)) (cons 1 (encode-symbol-bb msg right)))))))

    ; 2017-03-12 afternoon
    ; 2.69
    (define (adjoin-set-huffman-tree x set)
      (if (null? set) (list x)
        (cond ((< (weight x) (weight (car set))) (cons x set))
              (else (cons (car set) (adjoin-set-huffman-tree x (cdr set)))))))

    (define (make-leaf-set pairs)
      (if (null? pairs)
        null
        (adjoin-set-huffman-tree
          (make-leaf (car (car pairs))
                     (cadr (car pairs)))
          (make-leaf-set (cdr pairs)))))


    (define (generate-huffman-tree pairs)
      (successive-merge (make-leaf-set pairs)))

    (define (successive-merge orderset-leaves)
      (cond ((null? orderset-leaves) null)
            ((null? (cdr orderset-leaves)) orderset-leaves)
            (else
              (successive-merge
                (adjoin-set-huffman-tree
                  (make-code-tree
                    (car orderset-leaves)
                    (cadr orderset-leaves))
                  (cddr orderset-leaves))))))


    (define my-pairs
      (list
        (list 'A 4)
        (list 'B 2)
        (list 'C 1)
        (list 'D 1)))

    (define my-other-pairs
      (list
        (list 'A 8)
        (list 'B 3)
        (list 'C 1)
        (list 'D 1)
        (list 'E 1)
        (list 'F 1)
        (list 'G 1)
        (list 'H 1)))

    (define my-orderset-leaves
      (make-leaf-set my-pairs))


    ; 2017-03-13
    ; 2.70
    ; (define encode-lyrics

    ;   (define rock-song-pairs
    ;     (list
    ;       (list 'A 2)
    ;       (list 'GET 2)
    ;       (list 'SHA 3)
    ;       (list 'WAH 1)
    ;       (list 'BOOM 1)
    ;       (list 'JOB 2)
    ;       (list 'NA 16)
    ;       (list 'YIP 9)))

    ;   (define mk-tree
    ;     (generate-huffman-tree rock-song-pairs))

    ;   (define msg
    ;     '(Get a Job Sha na na na na))

    ;   (encode-bb msg mk-tree))

    ; 2.71 and 2.72 passed

    (define (make-complex-real-img x y)
      (cons x y))

    (define (complex-real z) (car z))

    (define (complex-img z) (cdr z))

    (define (add-complex z1 z2)
      (make-complex-real-img
        (+ (complex-real z1) (complex-real z2))
        (+ (complex-img z1) (complex-img z2))))

    (define (sub-complex z1 z2)
      (make-complex-real-img
        (- (complex-real z1) (complex-real z2))
        (- (complex-img z1) (complex-img z2))))

    (define (magn z) (sqrt (+ (square (complex-real z)) (square (complex-img z)))))

    (define (c-angle z) (atan (complex-img z) (complex-real z)))

    (define (make-complex-from-M-A m a)
      (make-complex-real-img
        (* m (cons a))
        (* m (sin a))))

    (define (mul-complex z1 z2)
      (make-complex-from-M-A
        (* (magn z1) (magn z2))
        (* (c-angle z1) (c-angle z2))))

    (define (div-complex z1 z2)
      (make-complex-from-M-A
        (/ (magn z1) (magn z2))
        (/ (c-angle z1) (c-angle z2))))
)

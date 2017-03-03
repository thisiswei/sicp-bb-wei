(module name racket
        (define (right-split painter n)
          (if (= n 0)
            painter
            (let ((splitted (right-split painter (- n 1))))
              (beside painter (below splitted splitted)))))


        (define (corner-split painter n)
          (if (= n 0)
            painter
            (let ((up-image (up-split painter (- n 1))))
              (beside
                (below painter (beside up-image up-image))
                (below (below
                         (right-split painter (- n 1))
                         (right-split painter (- n 1)))
                       (corner-split painter (- n 1)))))))

        ; 2.44
        (define (up-split painter n)
          (if (= n 0)
            painter
            (below
              painter
              (beside (up-split painter (- n 1)) (up-split painter (- n 1))))))

        ; (define (recursive-img painter n)
        ;   (if (= n 0)
        ;     painter
        ;     ; flip-vert
        ;     ; flip-horiz
        ;     ; (let ((corner-split-painter (corner-split painter n)))
        ;     ;   (flip-vert
        ;     ;     (flip-horiz
        ;     ;       corner-split-painter)))))
        ;     (let ((up-image (up-split ?? ??)))
        ;       (beside
        ;         (below (below
        ;                  (
        ;                (corner-split (recursive-img painter (- n 1)))


        ; (define right-split (split beside below))
        ; (define up-split (split below beside))
        ; 2.45
        (define (split s1 s2)
          (lambda (painter n)
            (cond ((= n 0) painter)
                  (else
                    (s1
                      painter
                      (s2 ((split s1 s2) painter (- n 1))
                          ((split s1 s2) painter (- n 1))))))))

        ; 2.46
        (define (make-vect x y)
          (cons x y))

        (define (xcor-vect v)
          (car v))

        (define (ycor-vet v)
          (cdr v))

        (define (add-vect v1 v2)
          (make-vect (+ (xcor-vect v1)
                        (xcor-vect v2))
                     (+ (ycor-vet v1)
                        (ycor-vet v2))))

        (define (sub-vect v1 v2)
          (make-vect (- (xcor-vect v1)
                        (xcor-vect v2))
                     (- (ycor-vet v1)
                        (ycor-vet v2))))

        (define (scale-vect v s)
          (make-vect (* (xcor-vect v) s)
                     (* (ycor-vect v) s)))

)

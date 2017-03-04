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

        ; 2.47
        (define (make-frame origin edge1 edge2)
          (list origin edge1 edge2))

        (define (origin-frame frame)
          (car frame))

        (define (edge1 frame)
          (cadr frame))

        (define (edge2 frame)
          (caddr frame))

        ; painters
        (define (segments->painter segment-list)
          (lambda (frame)
            (for-each
              (lambda (segment)
                (draw-line
                  ((frame-coor-map frame)
                   (start-segment segment))
                  ((frame-coor-map frame)
                   (end-segment segment))))
              segment-list)))

       ; 2.48
       (define (make-segment v1 v2)
         (cons v1 v2))

       (define (start-segment seg)
         (car seg))

       (define (end-segment seg)
         (cdr seg))

       ; 2.49
       ; think the solutions here for 2.49 are wrong
       ; a
       (define (outline frame)
         (let
           ((origin-opposite ((frame-coord-map frame)
                              (make-vect (xcor-vect (edge1 frame))
                                        (ycor-vect (edge2 frame))))))
           (segments->painter
             (list
               (make-segment (origin-frame frame) (edge1 frame))
               (make-segment (origin-frame frame) (edge2 frame))
               (make-segment (edge1 frame) origin-opposite)
               (make-segment (edge2 frame) origin-opposite)))))
       ; b
       (define (x-painter frame)
         (let
           ((origin-opposite
              ((frame-coord-map frame)
               (make-vect (xcor-vect (edge1 frame))
                          (ycor-vect (edge2 frame))))))
           (segments->painter
             (list
               (make-segment (origin-frame frame) origin-opposite)
               (make-segment (edge1 frame) (edge2 frame))))))

       ; c
       (define (diamond-painter frame)
         (let
           ; how to get the origin-opposite
           ((mid-lf (sub-vect (edge2 frame)
                              (origin-frame frame)))
            (mid-up (sub-vect origin-opposite
                              (edge2 frame)))
            (mid-down (sub-vect (edge1 frame)
                                (origin-frame frame)))
            (mid-rt (sub-vect origin-opposite
                              (edge1 frame))))
           (segments->painter
             (list
               (make-segment mid-lf mid-up)
               (make-segment mid-up mid-rt)
               (make-segment mid-rt mid-down)
               (make-segment mid-down mid-lf)))))

)

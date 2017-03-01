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
)

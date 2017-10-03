#lang racket

(define vec1 '#(1 2 3 4 5))
(define vec2 '#(5 4 3 2 1))
(define list1 '(1 2 3 4 5))
(define dupList '(1 1 2 2 2 2 3 4 4 3 3 5 5 2 2 2 1 5 5 4 5 6))


(define hypotenuse (lambda (x y)
                       (sqrt (+ (sqr x) (sqr y)))))
(define sphere (lambda (x)
                   (/ (* 4 pi (expt x 3)) 3)))

(define water (lambda (x)
                  (if (< x 32)
                      'Solid
                      (if (< x 212)
                          'Liquid
                          'Gas))))

(define dot-product(lambda(v1 v2)
  (foldl + 0 (vector->list (vector-map * v1 v2)))))

(define double-sequence(lambda (l1)
                         (map + l1 l1)))
(define duplicate(lambda (l1)
                   (append l1 l1)))
(define remove(lambda (l1)
                (remove-duplicates l1)))

(define dictionary '((one two three four five six seven eight nine ten I you fish red blue)
                     (uno dos tres quatro cinco seis siete ocho nueve dies yo tu pescado rojo azul)))
                   
(define translate (lambda (y)
                      (for-each (lambda (arg)
                                  (print (list-ref (cadr dictionary) (index-of (car dictionary) arg))))
                                y)))
(define sum(lambda (l1)
             (foldl + 0 l1)))

(define product(lambda (l1)
             (foldl * 1 l1)))

(define is-article(lambda (x)
                    (if (equal? x 'a) true
                        (if (equal? x 'an) true
                            (if (equal? x 'the) true false)))))

(define count-article(lambda (x)
                       (length (filter is-article x))))

(define is-color(lambda (x)
                  (if (equal? x 'black) true
                      (if (equal? x 'brown) true
                          (if (equal? x 'blue) true
                              (if (equal? x 'red) true
                                  (if (equal? x 'yellow) true
                                      (if (equal? x 'orange) true
                                          (if (equal? x 'purple) true
                                              (if (equal? x 'green) true
                                                  (if (equal? x 'gray) true
                                                      (if (equal? x 'pink) true false))))))))))))

(define Colors(lambda (x)
                (filter is-color x)))

(define Positives(lambda (x)
                   (filter positive? x)))
(define Max(lambda (x)
             (car (sort x >))))


(define squares(lambda (x)
                 (let ([y 0])
                   (for ([y x])
                     (writeln (* y y))))))

(define population (cons '(25 "abbigale")
                         (cons '(23 "bobbert")
                               (cons '(89 "carlos")
                                     (cons '(12 "daniel")
                                           (cons '(22 "everet")
                                                 (cons '(55 "frank") '())))))))

(define youngest(lambda ()
                  (cdr(car(sort population #:key car <)))))

(define sum-prod(lambda (x y)
                  (if (equal? x +)
                      (foldl x 0 y)
                      (foldl x 1 y))))


(define nest-level(lambda (ls current high)
                    (cond
                      [(and (list? (car ls)) (not(equal? (car ls) '()))) (set! high (nest-level (car ls) (+ current 1) high))]
                      [(equal? (car ls) '())(set! current (+ current 1))]
                      )
                    (cond
                      [(and (list? (cdr ls)) (not(equal? (cdr ls) '()))) (set! high (nest-level (cdr ls) current high))]
                      [(equal? (cdr ls) '()) (set! current (+ 1 current))]
                      )
                    (if (> current high)
                        current
                        high)))

(define runNest(lambda (x)
                 (write (nest-level x 0 0))))

(define find-level(lambda (ls current element)
                    (if (equal? (car ls) element)
                        (write (+ 1 current))
                        (cond
                           [(and (list? (car ls)) (not(equal? (car ls) '()))) (find-level (car ls) (+ current 1) element)]
                          ))
                    (cond[(and (list? (cdr ls)) (not(equal? (cdr ls) '()))) (find-level (cdr ls) current element)])))     
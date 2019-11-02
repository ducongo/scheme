
(define square (lambda(x)(* x x)))
(define double (lambda(x)(+ x x)))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (divisibleBy a)
  (lambda(x)
    (if (eq?(modulo x a) 0)
        #t
        #f)))

#|
(define (newmap f)
  (lambda (items)
    (if (null? items)
        '()
        (cons (f (car items))
              (newmap (cdr items))))))


(define test
  (lambda (x y)
    (define f
      (lambda z
        (if (> z 100)
            z
            (f (+ x y z)))))
    f))
|#

(define (newmap f)
  (lambda (items)
    (define foo
      (lambda  (items)
        (if (null? items)
            '()
            (cons (f (car items))
                  (foo (cdr items))))))
    (foo items)))

(define doubleAll (newmap (lambda(x)(* x 2))))


(doubleAll '(1 2 3 4))
(doubleAll '(10 20 30))

#|
part d and c
|#

(define (newfilter f)
  (lambda (sequence)
    (define foo
       (lambda (sequence)
         (cond ((null? sequence) '())
          ((f (car sequence)) 
		       (cons (f(car sequence)) 
                     (foo (cdr sequence))))
          (else (foo (cdr sequence))))))
    (foo sequence)))



(define (range a b)
    (if (= a b) (list a)
        (cons a (range (+ a 1) b))))



(define myfunc (newfilter
                (lambda (x)
                  (if (= (modulo x 4) 0)
                          (expt x 2)
                           #f ))))

(myfunc (range 1 20))


(define (maximum L)
  (define (foo current L)
    (if (null? L)
        current
        (if (<= current (car L))
               (foo (car L) (cdr L))
               (foo current (cdr L)))))
  (foo (car L) (cdr L)))
(maximum '(1 2 2 3 2 1))



(define (after L n)
  (define (foo L n current)
    (if (null? L)
        '()
        (if (> current n)
            (cons (car L)(foo (cdr L) n (+ current 1)))
            (foo (cdr L) n (+ current 1)))))
  (foo L n 1))

(after '(a b c d e f g h) 3)
(after '(a b c d e f g h) 0)


(define (tails L)
  (define (foo L L2 largest)
    (if (null? L)
        '()
        (if (< largest (car L))
            (foo L (cons (car L) (cdr L)) (car L))
            (foo (car L) L2 largest))))
  (foo (cdr L) '((car L)) (car L)))

(tails '(3 6 8 9 7 4 8 6 3))
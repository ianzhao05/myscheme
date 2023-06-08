mod common;
use common::assert_eval_eq;

#[test]
fn num_literals() {
    assert_eval_eq!("#d3e2", "300.0");
    assert_eval_eq!("#xface", "64206");
    assert_eval_eq!("#o105", "69");
    assert_eval_eq!("#b1010", "10");
    assert_eval_eq!("#e1.5", "3/2");
    assert_eval_eq!("#i3/2", "1.5");
    assert_eval_eq!("#e1.23e2", "123");
    assert_eval_eq!("5s2", "500.0");
    assert_eval_eq!("#e#xa/f", "10/15");
    assert_eval_eq!("#o#e1/10", "1/8");
}

#[test]
fn simple_defines() {
    assert_eval_eq!("(define x 1) x", "1");
    assert_eval_eq!("(define y (+ 2 3)) y", "5");
    assert_eval_eq!("(define z 0) (set! z 1) z", " 1");
    assert_eval_eq!(
        "(begin
          (begin (define a 1) (define b 2))
          (define c 3)
          (begin
            (define (f x) x)
            (define (g x) (f x))))
         a (f b) (g c)",
        "1 2 3"
    );
}

#[test]
fn ifs() {
    assert_eval_eq!("(if #t 1 2)", "1");
    assert_eval_eq!("(if '() 1 2)", "1");
    assert_eval_eq!("(if #f 1 2)", "2");
    assert_eval_eq!("(if #t 1)", "1");
    assert_eval_eq!("(if #f 1)", "");
}

#[test]
fn sequencing() {
    assert_eval_eq!("(begin 1 2 3)", "3");
    assert_eval_eq!("(let ((x 1)) (begin (set! x 2) x))", "2");
    assert_eval_eq!("(begin)", "");
    assert_eval_eq!("(begin (define x 5) (define (f g) (g x)) (f -))", "-5");
}

#[test]
fn ands_ors() {
    assert_eval_eq!("(and 1)", "1");
    assert_eval_eq!("(and 1 2)", "2");
    assert_eval_eq!("(and 1 2 #f 4 5)", "#f");
    assert_eval_eq!("(and 1 2 #f undef)", "#f");

    assert_eval_eq!("(or 1)", "1");
    assert_eval_eq!("(or 1 2)", "1");
    assert_eval_eq!("(or #f #f 3)", "3");
    assert_eval_eq!("(or #f #f #f)", "#f");
    assert_eval_eq!("(or (memq 'b '(a b c)) (/ 3 0))", "'(b c)");
}

#[test]
fn conds() {
    assert_eval_eq!("(cond (1))", "1");
    assert_eval_eq!("(cond (#t 1))", "1");
    assert_eval_eq!("(cond (#f 1) (#t 2))", "2");
    assert_eval_eq!("(cond (#f 1) (#f 2) (else 3))", "3");
    assert_eval_eq!("(cond ((cons 1 2) => car))", "1");
    assert_eval_eq!("(cond (#f => car) ((cons 1 2) => cdr))", "2");
}

#[test]
fn cases() {
    assert_eval_eq!(
        "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))",
        "'composite"
    );
    assert_eval_eq!("(case (car '(c d)) ((a) 'A) ((c) 'C) (else 'E))", "'C");
    assert_eval_eq!("(case 'b ((a) 'A) (else 'E))", "'E");
}

#[test]
fn lets() {
    assert_eval_eq!("(let ((x 2) (y 3)) (* x y))", "6");
    assert_eval_eq!(
        "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))",
        "35"
    );

    assert_eval_eq!(
        "(let f ((n 5) (acc 1)) (if (zero? n) acc (f (- n 1) (* n acc))))",
        "120"
    );
    assert_eval_eq!(
        "(let loop ((numbers '(3 -2 1 6 -5))
                    (nonneg '())
                    (neg ' () ))
            (cond ((null? numbers) (list nonneg neg))
                  ((>= (car numbers) 0)
                   (loop (cdr numbers) (cons (car numbers) nonneg) neg))
                  ((< (car numbers) 0)
                   (loop (cdr numbers) nonneg (cons (car numbers) neg)))))",
        "'((6 1 3) (-5 -2))"
    );

    assert_eval_eq!("(let* ((x 2) (y x)) (* x y))", "4");
    assert_eval_eq!(
        "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))",
        "70"
    );

    assert_eval_eq!(
        "(letrec ((f (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))) (f 5))",
        "120"
    );
    assert_eval_eq!(
        "(letrec ((my-even? (lambda (n) (if (zero? n) #t (my-odd? (- n 1)))))
                  (my-odd? (lambda (n) (if (zero? n) #f (my-even? (- n 1))))))
            (list (my-even? 42) (my-odd? 42)))",
        "'(#t #f)"
    );
}

#[test]
fn dos() {
    assert_eval_eq!("(do ((i 0 (+ i 1))) ((= i 3) i))", "3");

    assert_eval_eq!(
        "(do ((i 0 (+ i 1)) (j 0 (+ j 2)) (k 0))
           ((= i 3) (list i j k))
           (set! k (+ i j)))",
        "'(3 6 6)"
    );

    assert_eval_eq!(
        "(let ((x '(1 3 5 7 9)))
           (do ((x x (cdr x))
                (sum 0 (+ sum (car x))))
               ((null? x) sum)))",
        "25"
    );

    assert_eval_eq!(
        "(do ((vec (make-vector 5))
              (i 0 (+ i 1)))
             ((= i 5) vec)
           (vector-set! vec i i))",
        "'#(0 1 2 3 4)"
    );

    assert_eval_eq!("(do () (#t))", "");
}

#[test]
fn quasiquotes() {
    assert_eval_eq!("`(1 2 3)", "'(1 2 3)");
    assert_eval_eq!("`(list ,(+ 1 2) 4)", "'(list 3 4)");
    assert_eval_eq!("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", "'(a 3 4 5 6 b)");
    assert_eval_eq!(
        "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))",
        "'((foo 7) . cons)"
    );
    assert_eval_eq!(
        "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)",
        "'#(10 5 2 4 3 8)"
    );
    assert_eval_eq!(
        "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) ,@(list 5 6) d) e) f)",
        "'(a `(b ,(+ 1 2) ,(foo 4 5 6 d) e) f)"
    );
    assert_eval_eq!(
        "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))",
        "'(a `(b ,x ,'y d) e)"
    );
    assert_eval_eq!(
        "(define abc '(a b c)) ``(,,@abc) ``#(,@,@abc)",
        "'`((unquote a b c)) '`#((unquote-splicing a b c))"
    );
}

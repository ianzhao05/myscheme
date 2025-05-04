mod common;
use common::assert_eval_eq;

#[test]
fn simple_procs() {
    assert_eval_eq!("(define (f x) (+ x 1)) (f 1)", "2");
    assert_eval_eq!("(define (g x y) (+ x y)) (g 1 2)", "3");
    assert_eval_eq!("(define (h x . y) (+ x (car y))) (h 1 2 3)", "3");
    assert_eval_eq!(
        "(define j (lambda x (+ (car x) (car (cdr x))))) (j 1 2)",
        "3"
    );
}

#[test]
fn recursive_procs() {
    assert_eval_eq!(
        "(define (fact n) (if (zero? n) 1 (* n (fact (- n 1))))) (fact 5) (fact 10)",
        "120 3628800"
    );

    assert_eval_eq!(
        "(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) (fact 8)",
        "40320"
    );

    assert_eval_eq!(
        "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)",
        "89"
    );

    assert_eval_eq!(
        "(define (sum l) (if (null? l) 0 (+ (car l) (sum (cdr l))))) (sum '(1 2 3 4 5))",
        "15"
    );

    assert_eval_eq!(
        "(define (last l) (if (null? (cdr l)) (car l) (last (cdr l)))) (last '(1 2 3 4 5))",
        "5"
    );

    assert_eval_eq!(
        "(define (my-reverse l acc) (if (null? l) acc (my-reverse (cdr l) (cons (car l) acc))))
         (my-reverse '(1 2 3 4 5) '())",
        "'(5 4 3 2 1)"
    );

    assert_eval_eq!(
        "(define (Y f) ((lambda (x) (x x)) (lambda (x) (f (lambda (v) ((x x) v))))))
         (define (fact self) (lambda (n) (if (zero? n) 1 (* n (self (- n 1))))))
         ((Y fact) 5)",
        "120"
    );
}

#[test]
fn closures() {
    assert_eval_eq!(
        "(define count ((lambda (next) (lambda () (set! next (+ next 1)) next)) 0))
         (count)
         (count)
         (count)",
        "1 2 3"
    );

    assert_eval_eq!(
        "(define (make-adder x) (lambda (y) (+ x y)))
         (define add2 (make-adder 2))
         (add2 3)",
        "5"
    );
}

mod common;
use common::assert_eval_eq;

#[test]
fn control() {
    assert_eval_eq!("(apply + '(4 3))", "7");
    assert_eval_eq!("(apply - 3 '(2 1))", "0");
    assert_eval_eq!("(apply list 1 2 3 '(4 5 6))", "'(1 2 3 4 5 6)");

    assert_eval_eq!("(map (lambda (x) (* x x)) '(1 2 3 4))", "'(1 4 9 16)");
    assert_eval_eq!("(map + '(1 2 3) '(4 5 6))", "'(5 7 9)");
    assert_eval_eq!(
        "(map list '(1 2 3) '(4 5 6) '(7 8 9))",
        "'((1 4 7) (2 5 8) (3 6 9))"
    );

    assert_eval_eq!(
        "(let ((count 0))
           (for-each (lambda (x) (set! count (+ count x))) '(1 2 3))
           count)",
        "6"
    );
}

#[test]
fn callcc() {
    assert_eval_eq!("(call-with-current-continuation (lambda (k) (+ 2 5)))", "7");
    assert_eval_eq!(
        "(call-with-current-continuation (lambda (k) (+ 2 (k 3) 5)))",
        "3"
    );

    assert_eval_eq!(
        "((let ((comeback (call-with-current-continuation (lambda (c) c))))
            (comeback (lambda (x) x)))
          5)",
        "5"
    );

    assert_eval_eq!(
        "(define (gen lst)
           (define (state ret)
             (for-each
              (lambda (x)
               (set! ret (call-with-current-continuation
                          (lambda (res) (set! state res) (ret x)))))
              lst)
             (ret 'end))
           (lambda () (call-with-current-continuation state)))

         (define g (gen '(0 1 2)))
         (g) (g) (g) (g)",
        "0 1 2 'end"
    );

    assert_eval_eq!("(call-with-current-continuation procedure?)", "#t");

    assert_eval_eq!(
        "(define c (call-with-current-continuation (lambda (c) c)))
         (procedure? c)
         (c 99) c",
        "#t 99"
    );
}

#[test]
fn eval() {
    assert_eval_eq!(
        "(let ((a 3) (b 7)) (eval `(* ,a ,b) (scheme-report-environment 5)))",
        "21"
    );
    assert_eval_eq!(
        "(let ((f (eval '(lambda (f x) (f x x)) (null-environment 5))))
           (f + 10))",
        "20"
    );
}

#[test]
fn delays() {
    assert_eval_eq!("(force (delay (+ 1 2)))", "3");

    assert_eval_eq!(
        "(let ((p (delay (+ 1 2)))) (list (force p) (force p)))",
        "'(3 3)"
    );

    assert_eval_eq!(
        "(let ((p (delay (+ 1 2)))) (set! p (delay (+ 3 4))) (force p))",
        "7"
    );

    assert_eval_eq!(
        "(define a-stream
           (letrec ((next (lambda (n) (cons n (delay (next (+ n 1)))))))
             (next 0)))
         (define head car)
         (define (tail stream) (force (cdr stream)))
         (head (tail (tail a-stream)))",
        "2"
    );

    assert_eval_eq!(
        "(define count 0)
         (define x 5)
         (define p
           (delay (begin (set! count (+ count 1))
                         (if (> count x) count (force p)))))
         (force p)
         (begin (set! x 10) (force p))",
        "6 6"
    )
}

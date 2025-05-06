mod common;
use common::assert_eval_eq;

#[test]
fn hygiene() {
    assert_eval_eq!(
        "(let ((=> #f))
           (cond (#t => 'ok)))",
        "'ok"
    );

    assert_eval_eq!(
        "(define-syntax my-or
           (syntax-rules ()
             ((my-or) #f)
             ((my-or e) e)
             ((my-or e1 e2 ...)
               (let ((temp e1))
                 (if temp temp
                     (my-or e2 ...))))))
        (let ((x #f)
               (y 7)
               (temp 8)
               (let odd?)
               (if even?))
           (my-or x
                  (let temp)
                  (if y)
                  y))",
        "7"
    );
}

#[test]
fn ellipses() {
    assert_eval_eq!(
        "(define-syntax foo
           (syntax-rules ()
             ((foo a ...) '(a ...))))
         (foo x y z)",
        "'(x y z)"
    );

    assert_eval_eq!(
        "(define-syntax foo
           (syntax-rules ()
             ((foo (x ...) (y ...)) '((x y) ...))))
         (foo (q w e r) (a s d f))",
        "'((q a) (w s) (e d) (r f))"
    );

    assert_eval_eq!(
        "(define-syntax foo
           (syntax-rules ()
             ((foo ((x ...) ...) (y ...)) '((x ... y) ...))))
         (foo ((q 0 (q 1)) (w 2 (w 3)) (e (4 e) 5) (r 6 (r 7)) ((t 8) t 9)) (a s d f g))",
        "'((q 0 (q 1) a) (w 2 (w 3) s) (e (4 e) 5 d) (r 6 (r 7) f) ((t 8) t 9 g))"
    );

    assert_eval_eq!(
        "(define-syntax foo
           (syntax-rules ()
             ((foo ((a ...) ...) (b ...) c) '(((a ...) b c) ...))))
         (foo ((1 a b c) (2 b) (3 c)) (4 5 6) x)",
        "'(((1 a b c) 4 x) ((2 b) 5 x) ((3 c) 6 x))"
    );
}

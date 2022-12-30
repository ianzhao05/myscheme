use myscheme::env::Env;

macro_rules! assert_eval_eq {
    ($lhs:expr, $rhs:expr, $env:expr) => {{
        use myscheme::{eval_str, evaler::EvalResult::Expr, object::ObjectRef};
        let lv: Vec<_> = eval_str($lhs, $env.clone())
            .unwrap()
            .into_iter()
            .filter_map(|r| match r {
                Expr(o) => Some(o),
                _ => None,
            })
            .collect();
        let rv: Vec<_> = eval_str($lhs, $env.clone())
            .unwrap()
            .into_iter()
            .filter_map(|r| match r {
                Expr(o) => Some(o),
                _ => None,
            })
            .collect();
        assert!(lv.len() == rv.len());
        assert!(lv
            .iter()
            .zip(rv.iter())
            .all(|(l, r)| ObjectRef::equal(l, r)));
    }};
}

#[test]
fn arithmetic() {
    let env = Env::primitives();

    assert_eval_eq!("(+)", "0", env);
    assert_eval_eq!("(+ 1 2)", "3", env);
    assert_eval_eq!("(+ 1 5/2 5/2 4 5)", "15", env);
    assert_eval_eq!("(+ 1 2 3 4 5.0)", "15.0", env);

    assert_eval_eq!("(- 1)", "-1", env);
    assert_eval_eq!("(- 10 5)", "5", env);
    assert_eval_eq!("(- 10 5 10.0)", "-5.0", env);
    assert_eval_eq!("(- 20/3 7/10)", "179/30", env);

    assert_eval_eq!("(*)", "1", env);
    assert_eval_eq!("(* 1 2)", "2", env);
    assert_eval_eq!("(* 1 2 3 4 5)", "120", env);
    assert_eval_eq!("(* 1 2 3 4 5.0)", "120.0", env);
    assert_eval_eq!("(* 3/4 3/4)", "9/16", env);

    assert_eval_eq!("(/ 1)", "1", env);
    assert_eval_eq!("(/ 10 5)", "2", env);
    assert_eval_eq!("(/ 10 5 2)", "1", env);
    assert_eval_eq!("(/ 20 3 2)", "10/3", env);
    assert_eval_eq!("(/ 1.0 2)", "0.5", env);
}

#[test]
fn list_primitives() {
    let env = Env::primitives();

    assert_eval_eq!("(cons 1 2)", "'(1 . 2)", env);
    assert_eval_eq!("(car (cons 1 2))", "1", env);
    assert_eval_eq!("(cdr (cons 1 2))", "2", env);
    assert_eval_eq!("(list 1 2 3)", "'(1 2 3)", env);

    assert_eval_eq!("(null? '())", "#t", env);
    assert_eval_eq!("(null? '(1 2 3))", "#f", env);
}

#[test]
fn simple_defines() {
    let env = Env::primitives();

    assert_eval_eq!("(define x 1) x", "1", env);
    assert_eval_eq!("(define y (+ 2 3)) y", "5", env);
    assert_eval_eq!("(define z (+ x y)) z", "6", env);
}

#[test]
fn simple_procs() {
    let env = Env::primitives();
    assert_eval_eq!("(define (f x) (+ x 1)) (f 1)", "2", env);
    assert_eval_eq!("(define (g x y) (+ x y)) (g 1 2)", "3", env);
    assert_eval_eq!("(define (h x . y) (+ x (car y))) (h 1 2 3)", "3", env);
    assert_eval_eq!(
        "(define j (lambda x (g (car x) (car (cdr x))))) (j 1 2)",
        "3",
        env
    );
}

#[test]
fn recursive_procs() {
    let env = Env::primitives();

    assert_eval_eq!(
        "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)",
        "120",
        env
    );
    assert_eval_eq!("(fact 10)", "3628800", env);

    assert_eval_eq!(
        "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)",
        "55",
        env
    );

    assert_eval_eq!(
        "(define (sum l) (if (null? l) 0 (+ (car l) (sum (cdr l))))) (sum '(1 2 3 4 5))",
        "15",
        env
    );

    assert_eval_eq!(
        "(define (last l) (if (null? (cdr l)) (car l) (last (cdr l)))) (last '(1 2 3 4 5))",
        "5",
        env
    );

    assert_eval_eq!(
        "(define (reverse l acc) (if (null? l) acc (reverse (cdr l) (cons (car l) acc)))) (reverse '(1 2 3 4 5) '())",
        "'(5 4 3 2 1)",
        env
    );

    assert_eval_eq!(
        "(define (Y f) ((lambda (x) (x x)) (lambda (x) (f (lambda (v) ((x x) v))))))
         (define (fact self) (lambda (n) (if (= n 0) 1 (* n (self (- n 1))))))
         ((Y fact) 5)",
        "120",
        env
    );
}

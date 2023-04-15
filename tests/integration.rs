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
        let rv: Vec<_> = eval_str($rhs, $env.clone())
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
fn num_primitives() {
    let env = Env::primitives();

    assert_eval_eq!("(zero? 0)", "#t", env);
    assert_eval_eq!("(zero? 1)", "#f", env);
    assert_eval_eq!("(zero? -1)", "#f", env);

    assert_eval_eq!("(positive? 0)", "#f", env);
    assert_eval_eq!("(positive? 1)", "#t", env);
    assert_eval_eq!("(positive? -1)", "#f", env);

    assert_eval_eq!("(negative? 0)", "#f", env);
    assert_eval_eq!("(negative? 1)", "#f", env);
    assert_eval_eq!("(negative? -1)", "#t", env);
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

    assert_eval_eq!("(length '())", "0", env);
    assert_eval_eq!("(length '(1 2 3))", "3", env);

    assert_eval_eq!("(reverse '())", "'()", env);
    assert_eval_eq!("(reverse '(1 2 3))", "'(3 2 1)", env);

    assert_eval_eq!("(memq 'a '(a b c))", "'(a b c)", env);
    assert_eval_eq!("(memq 'b '(a b c))", "'(b c)", env);
    assert_eval_eq!("(memq 'c '(a b c))", "'(c)", env);
    assert_eval_eq!("(memq 'd '(a b c))", "#f", env);

    assert_eval_eq!("(memv 2 '(1 2 3))", "'(2 3)", env);
    assert_eval_eq!("(memv 4 '(1 2 3))", "#f", env);

    assert_eval_eq!("(member '(4 5) '((1 2 3) (4 5) (6)))", "'((4 5) (6))", env);
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
        "(define (fact n) (if (zero? n) 1 (* n (fact (- n 1))))) (fact 5)",
        "120",
        env
    );
    assert_eval_eq!("(fact 10)", "3628800", env);

    assert_eval_eq!(
        "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)",
        "89",
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
        "(define (my-reverse l acc) (if (null? l) acc (my-reverse (cdr l) (cons (car l) acc))))
         (my-reverse '(1 2 3 4 5) '())",
        "'(5 4 3 2 1)",
        env
    );

    assert_eval_eq!(
        "(define (Y f) ((lambda (x) (x x)) (lambda (x) (f (lambda (v) ((x x) v))))))
         (define (fact self) (lambda (n) (if (zero? n) 1 (* n (self (- n 1))))))
         ((Y fact) 5)",
        "120",
        env
    );
}

#[test]
fn closures() {
    let env = Env::primitives();

    assert_eval_eq!(
        "(define count ((lambda (next) (lambda () (set! next (+ next 1)) next)) 0))
         (count)
         (count)
         (count)",
        "1 2 3",
        env
    )
}

#[test]
fn ands_ors() {
    let env = Env::primitives();

    assert_eval_eq!("(and 1)", "1", env);
    assert_eval_eq!("(and 1 2)", "2", env);
    assert_eval_eq!("(and 1 2 #f 4 5)", "#f", env);

    assert_eval_eq!("(or 1)", "1", env);
    assert_eval_eq!("(or 1 2)", "1", env);
    assert_eval_eq!("(or #f #f 3)", "3", env);
    assert_eval_eq!("(or #f #f #f)", "#f", env);
}

#[test]
fn conds() {
    let env = Env::primitives();

    assert_eval_eq!("(cond (1))", "1", env);
    assert_eval_eq!("(cond (#t 1))", "1", env);
    assert_eval_eq!("(cond (#f 1) (#t 2))", "2", env);
    assert_eval_eq!("(cond (#f 1) (#f 2) (else 3))", "3", env);
    assert_eval_eq!("(cond ((cons 1 2) => car))", "1", env);
    assert_eval_eq!("(cond (#f => car) ((cons 1 2) => cdr))", "2", env);
}

#[test]
fn cases() {
    let env = Env::primitives();

    assert_eval_eq!(
        "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))",
        "'composite",
        env
    );
    assert_eval_eq!("(case (car '(c d)) ((a) 'A) ((c) 'C) (else 'E))", "'C", env);
    assert_eval_eq!("(case 'b ((a) 'A) (else 'E))", "'E", env);
}

#[test]
fn lets() {
    let env = Env::primitives();

    assert_eval_eq!("(let ((x 2) (y 3)) (* x y))", "6", env);
    assert_eval_eq!(
        "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))",
        "35",
        env
    );

    assert_eval_eq!(
        "(let f ((n 5) (acc 1)) (if (zero? n) acc (f (- n 1) (* n acc))))",
        "120",
        env
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
        "'((6 1 3) (-5 -2))",
        env
    );

    assert_eval_eq!("(let* ((x 2) (y x)) (* x y))", "4", env);
    assert_eval_eq!(
        "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))",
        "70",
        env
    );

    assert_eval_eq!(
        "(letrec ((f (lambda (n) (if (zero? n) 1 (* n (f (- n 1))))))) (f 5))",
        "120",
        env
    );
    assert_eval_eq!(
        "(letrec ((my-even? (lambda (n) (if (zero? n) #t (my-odd? (- n 1)))))
                  (my-odd? (lambda (n) (if (zero? n) #f (my-even? (- n 1))))))
            (list (my-even? 42) (my-odd? 42)))",
        "'(#t #f)",
        env
    );
}

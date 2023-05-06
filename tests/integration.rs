macro_rules! assert_eval_eq {
    ($lhs:expr, $rhs:expr) => {{
        use myscheme::{env::Env, eval_str, evaler::EvalResult::Expr, object::ObjectRef};
        let env = Env::primitives();
        let lv: Vec<_> = eval_str($lhs, env.clone())
            .unwrap()
            .into_iter()
            .filter_map(|r| match r {
                Expr(o) => Some(o),
                _ => None,
            })
            .collect();
        let rv: Vec<_> = eval_str($rhs, env)
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
fn simple_defines() {
    assert_eval_eq!("(define x 1) x", "1");
    assert_eval_eq!("(define y (+ 2 3)) y", "5");
    assert_eval_eq!("(define z 0) (set! z 1) z", "(if #f #f) 1");
}

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

#[test]
fn sequencing() {
    assert_eval_eq!("(begin 1 2 3)", "3");
    assert_eval_eq!("(let ((x 1)) (begin (set! x 2) x))", "2");
    assert_eval_eq!("(begin)", "(if #f #f)");
}

#[test]
fn ands_ors() {
    assert_eval_eq!("(and 1)", "1");
    assert_eval_eq!("(and 1 2)", "2");
    assert_eval_eq!("(and 1 2 #f 4 5)", "#f");

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

    assert_eval_eq!("(do () (#t))", "(if #f #f)");
}

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
}

#[test]
fn predicates() {
    assert_eval_eq!("(not 5)", "#f");
    assert_eval_eq!("(not #f)", "#t");

    assert_eval_eq!("(number? 3)", "#t");
    assert_eval_eq!("(number? 6/10)", "#t");
    assert_eval_eq!("(number? 3.5)", "#t");
    assert_eval_eq!("(number? (lambda () 10))", "#f");

    assert_eval_eq!("(boolean? #t)", "#t");
    assert_eval_eq!("(boolean? #f)", "#t");
    assert_eval_eq!("(boolean? 1)", "#f");
    assert_eval_eq!("(boolean? '())", "#f");

    assert_eval_eq!("(symbol? 'a)", "#t");
    assert_eval_eq!("(symbol? '())", "#f");

    assert_eval_eq!("(char? #\\a)", "#t");
    assert_eval_eq!("(char? #\\newline)", "#t");
    assert_eval_eq!("(char? \"a\")", "#f");

    assert_eval_eq!("(string? \"a\")", "#t");
    assert_eval_eq!("(string? 'a)", "#f");

    assert_eval_eq!("(vector? '#(1 2 3))", "#t");
    assert_eval_eq!("(vector? '(1 2 3))", "#f");

    assert_eval_eq!("(procedure? (lambda () 0))", "#t");
    assert_eval_eq!("(define (f x) x) (procedure? f)", "#t");

    assert_eval_eq!("(null? '())", "#t");
    assert_eval_eq!("(null? '(1 2 3))", "#f");

    assert_eval_eq!("(pair? 1)", "#f");
    assert_eval_eq!("(pair? '())", "#f");
    assert_eval_eq!("(pair? '(1 2 3))", "#t");
    assert_eval_eq!("(pair? '(1 . 2))", "#t");
}

#[test]
fn arithmetic() {
    assert_eval_eq!("(+)", "0");
    assert_eval_eq!("(+ 1 2)", "3");
    assert_eval_eq!("(+ 1 5/2 5/2 4 5)", "15");
    assert_eval_eq!("(+ 1 2 3 4 5.0)", "15.0");

    assert_eval_eq!("(- 1)", "-1");
    assert_eval_eq!("(- 10 5)", "5");
    assert_eval_eq!("(- 10 5 10.0)", "-5.0");
    assert_eval_eq!("(- 20/3 7/10)", "179/30");

    assert_eval_eq!("(*)", "1");
    assert_eval_eq!("(* 1 2)", "2");
    assert_eval_eq!("(* 1 2 3 4 5)", "120");
    assert_eval_eq!("(* 1 2 3 4 5.0)", "120.0");
    assert_eval_eq!("(* 3/4 3/4)", "9/16");

    assert_eval_eq!("(/ 1)", "1");
    assert_eval_eq!("(/ 10 5)", "2");
    assert_eval_eq!("(/ 10 5 2)", "1");
    assert_eval_eq!("(/ 20 3 2)", "10/3");
    assert_eval_eq!("(/ 1.0 2)", "0.5");
}

#[test]
fn num_primitives() {
    assert_eval_eq!("(integer? 3)", "#t");
    assert_eval_eq!("(integer? 3.0)", "#t");
    assert_eval_eq!("(integer? 8/4)", "#t");
    assert_eval_eq!("(integer? #t)", "#f");

    assert_eval_eq!("(exact? 3)", "#t");
    assert_eval_eq!("(exact? 3.0)", "#f");
    assert_eval_eq!("(exact? 8/5)", "#t");
    assert_eval_eq!("(inexact? 5.3)", "#t");

    assert_eval_eq!("(zero? 0)", "#t");
    assert_eval_eq!("(zero? 1)", "#f");
    assert_eval_eq!("(zero? -1)", "#f");

    assert_eval_eq!("(positive? 0)", "#f");
    assert_eval_eq!("(positive? 1)", "#t");
    assert_eval_eq!("(positive? -1)", "#f");

    assert_eval_eq!("(negative? 0)", "#f");
    assert_eval_eq!("(negative? 1)", "#f");
    assert_eval_eq!("(negative? -1)", "#t");

    assert_eval_eq!("(max 1 2 3 4 5)", "5");
    assert_eval_eq!("(max 1 2 3.0 4 5)", "5.0");

    assert_eval_eq!("(min 1 2 -3 4 5)", "-3");
    assert_eval_eq!("(min 1 2 -3 4.0 5)", "-3.0");
}

#[test]
fn list_primitives() {
    assert_eval_eq!("(cons 1 2)", "'(1 . 2)");
    assert_eval_eq!("(car (cons 1 2))", "1");
    assert_eval_eq!("(cdr (cons 1 2))", "2");
    assert_eval_eq!("(list 1 2 3)", "'(1 2 3)");

    assert_eval_eq!("(list? '())", "#t");
    assert_eval_eq!("(list? '(1 2 3))", "#t");
    assert_eval_eq!("(list? '(1 2 . 3))", "#f");

    assert_eval_eq!("(cadadr '((a b) (c d) (e f)))", "'d");

    assert_eval_eq!("(let ((x '(1 . 2))) (set-car! x 3) (car x))", "3");
    assert_eval_eq!("(let ((x '(1 . 2))) (set-cdr! x 3) (cdr x))", "3");

    assert_eval_eq!("(length '())", "0");
    assert_eval_eq!("(length '(1 2 3))", "3");

    assert_eval_eq!("(reverse '())", "'()");
    assert_eval_eq!("(reverse '(1 2 3))", "'(3 2 1)");

    assert_eval_eq!("(append '(a b c) '())", "'(a b c)");
    assert_eval_eq!("(append '() '(a b c))", "'(a b c)");
    assert_eval_eq!("(append '(a b) '(c d) '(e f))", "'(a b c d e f)");
    assert_eval_eq!("(append '(a b) 'c)", "'(a b . c)");
    assert_eval_eq!("(append '(a b) '(c . d))", "'(a b c . d)");
    assert_eval_eq!("(let ((x '(b))) (eq? x (cdr (append '(a) x))))  ", "#t");

    assert_eval_eq!("(list-tail '(a b c d) 1)", "'(b c d)");
    assert_eval_eq!("(list-ref '(a b c d) 2)", "'c");

    assert_eval_eq!(
        "(define ml '(a b c))
         (memq 'a ml)
         (memq 'b ml)
         (memq 'c ml)
         (memq 'd ml)",
        "'(a b c) '(b c) '(c) #f"
    );
    assert_eval_eq!("(memv 2 '(1 2 3)) (memv 4 '(1 2 3))", "'(2 3) #f");
    assert_eval_eq!("(member '(4 5) '((1 2 3) (4 5) (6)))", "'((4 5) (6))");

    assert_eval_eq!(
        "(define al '((a 1) (b 2) (c 3)))
         (assq 'a al)
         (assq 'b al)
         (assq 'd al)",
        "'(a 1) '(b 2) #f"
    );
    assert_eval_eq!("(assv 5 '((2 3) (5 7) (11 13)))", "'(5 7)");
    assert_eval_eq!("(assoc '(a) '(((a)) ((b)) ((c))))", "'((a))");
}

#[test]
fn vector_primitives() {
    assert_eval_eq!("(vector-length (make-vector 3))", "3");
    assert_eval_eq!("(make-vector 3 0)", "'#(0 0 0)");

    assert_eval_eq!("(vector-ref '#(1 2 3) 1)", "2");
    assert_eval_eq!(
        "(define v '#(0 0 0)) (begin (vector-set! v 1 10) v)",
        "'#(0 10 0)"
    );
    assert_eval_eq!(
        "(define v '#(1 2 3)) (begin (vector-fill! v 10) v)",
        "'#(10 10 10)"
    );

    assert_eval_eq!("(vector 1 2 3)", "'#(1 2 3)");
    assert_eval_eq!("(vector->list '#(1 2 3))", "'(1 2 3)");
    assert_eval_eq!("(list->vector '(1 2 3))", "'#(1 2 3)");
}

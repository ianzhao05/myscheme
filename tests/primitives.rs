mod common;
use common::assert_eval_eq;

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
fn equivalence() {
    assert_eval_eq!("(eqv? 'a 'a)", "#t");
    assert_eval_eq!("(eqv? 'a 'b)", "#f");
    assert_eval_eq!("(eqv? 2 2)", "#t");
    assert_eval_eq!("(eqv? '() '())", "#t");
    assert_eval_eq!("(eqv? 100000000 100000000)", "#t");
    assert_eval_eq!("(eqv? (cons 1 2) (cons 1 2))", "#f");
    assert_eval_eq!("(eqv? (lambda () 1) (lambda () 2))", "#f");
    assert_eval_eq!("(eqv? car car)", "#t");
    assert_eval_eq!("(eqv? cadr cadr)", "#t");
    assert_eval_eq!("(eqv? #f 'nil)", "#f");
    assert_eval_eq!("(let ((p (lambda (x) x))) (eqv? p p))", "#t");
    assert_eval_eq!("(let ((p (cons 1 2))) (eqv? p p))", "#t");

    assert_eval_eq!("(equal? '(a (b) c) '(a (b) c))", "#t");
    assert_eval_eq!("(equal? 2 2)", "#t");
    assert_eval_eq!("(equal? (make-vector 5 'a) (make-vector 5 'a))", "#t");
    assert_eval_eq!("(equal? '#((1 2) #(3 4)) '#((1 2) #(3 4)))", "#t");
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

#[test]
fn string_primitives() {
    assert_eval_eq!(r"(make-string 3 #\a)", "\"aaa\"");
    assert_eval_eq!(r"(string #\a #\b #\c)", "\"abc\"");
    assert_eval_eq!("(string-length \"abc\")", "3");

    assert_eval_eq!("(string-ref \"abc\" 1)", "#\\b");
    assert_eval_eq!(r#"(define s "abc") (string-set! s 1 #\d) s"#, "\"adc\"");
    assert_eval_eq!(r#"(define s "abc") (string-fill! s #\1) s"#, "\"111\"");

    assert_eval_eq!("(string=? \"abc\" \"abc\")", "#t");
    assert_eval_eq!("(string=? \"abc\" \"ABC\")", "#f");
    assert_eval_eq!("(string-ci=? \"abc\" \"ABC\")", "#t");

    assert_eval_eq!("(string<? \"abc\" \"abd\")", "#t");
    assert_eval_eq!("(string<? \"abc\" \"abb\")", "#f");

    assert_eval_eq!("(string<? \"abc\" \"abc\")", "#f");
    assert_eval_eq!("(string<=? \"abc\" \"abc\")", "#t");

    assert_eval_eq!("(string<? \"abc\" \"abcde\")", "#t");

    assert_eval_eq!("(string<? \"Abc\" \"abc\")", "#t");
    assert_eval_eq!("(string-ci<? \"Abc\" \"abc\")", "#f");

    assert_eval_eq!("(string-copy \"abcde\")", "\"abcde\"");
    assert_eval_eq!("(substring \"abcde\" 1 3)", "\"bc\"");

    assert_eval_eq!("(string->list \"abc\")", r"'(#\a #\b #\c)");
    assert_eval_eq!(r"(list->string '(#\a #\b #\c))", "\"abc\"");
}

#[test]
fn symbol_primitives() {
    assert_eval_eq!("(symbol->string 'symbol)", "\"symbol\"");
    assert_eval_eq!("(string->symbol \"string\")", "'string");
    assert_eval_eq!("(eq? 'abc (string->symbol \"abc\"))", "#t");
}

#[test]
fn io_primitives() {
    use assert_fs::prelude::*;

    let tmp_dir = assert_fs::TempDir::new().unwrap();

    let outfile = tmp_dir.child("outfile.txt");
    assert_eval_eq!(
        &format!(
            r#"
        (call-with-output-file
          "{}"
          (lambda (p)
            (do ((data '(42 (#\a #\space) #("a\\b" "a\"b") (a (1 (2 3) 4) () . b)) (cdr data)))
                ((null? data))
              (display (car data) p)
              (newline p)
              (write (car data) p)
              (newline p))
            (write-char #\a p)
            (write-char #\b p)
            (write-char #\c p)))
             "#,
            outfile.path().display()
        ),
        ""
    );
    outfile.assert(
        r#"42
42
(a  )
(#\a #\space)
#(a\b a"b)
#("a\\b" "a\"b")
(a (1 (2 3) 4) () . b)
(a (1 (2 3) 4) () . b)
abc"#,
    );

    let infile = tmp_dir.child("infile.txt");
    infile
        .write_str(
            r#"hello world
        (* (+ 4 5) (- 7 3)
         #(1 2 3 4)
         abc "Hello,
world!" (


         #\a)) '(

        ) `
         (a ,b
        ,@
        (1 2 3))

        "#,
        )
        .unwrap();
    assert_eval_eq!(
        &format!(
            r#"
        (call-with-input-file
          "{}"
          (lambda (p)
            (define res '())
            (define (add x) (set! res (cons x res)))
            (add (read-char p))
            (add (read p))
            (add (peek-char p))
            (add (read-char p))
            (add (read p))
            (add (read-char p))
            (add (read p))
            (add (read p))
            (add (read p))
            (add (eof-object? (read p)))
            (add (eof-object? (read-char p)))
            (add (eof-object? (peek-char p)))
            (reverse res)))
             "#,
            infile.path().display()
        ),
        r#"(list
             #\h 'ello #\space #\space 'world #\newline
             '(* (+ 4 5) (- 7 3) #(1 2 3 4) abc "Hello,
world!" (#\a))
             ''()
             '`(a ,b ,@(1 2 3))
             #t #t #t)"#
    );

    tmp_dir.close().unwrap();
}

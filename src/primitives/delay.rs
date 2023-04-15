pub const PRELUDE: &str = "
(define (make-promise proc)
  (let ((result-ready? #f)
        (result #f))
    (lambda ()
      (if result-ready?
          result
          (let ((x (proc)))
            (if result-ready?
                result
                (begin
                  (set! result-ready? #t)
                  (set! result x)
                  result)))))))

(define (force promise) (promise))
";

mod common;
use common::assert_eval_eq;

#[test]
fn cond_hygiene() {
    assert_eval_eq!(
        "(let ((=> #f))
           (cond (#t => 'ok)))",
        "'ok"
    );
}

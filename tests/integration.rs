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
fn test() {
    let env = Env::primitives();
    assert_eval_eq!("(+ 1 2)", "3", env);
}

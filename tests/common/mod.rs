macro_rules! assert_eval_eq {
    ($lhs:expr, $rhs:expr) => {{
        use myscheme::{env::primitive_env, eval_str, object::ObjectRef};
        let lv: Vec<_> = eval_str($lhs, &primitive_env())
            .unwrap()
            .into_iter()
            .filter_map(|r| match r {
                ObjectRef::Void => None,
                _ => Some(r),
            })
            .collect();
        let rv: Vec<_> = eval_str($rhs, &primitive_env())
            .unwrap()
            .into_iter()
            .filter_map(|r| match r {
                ObjectRef::Void => None,
                _ => Some(r),
            })
            .collect();
        assert!(
            lv.len() == rv.len(),
            "Expressions produced different number of results"
        );
        assert!(
            lv.iter()
                .zip(rv.iter())
                .all(|(l, r)| ObjectRef::equal(l, r)),
            "Expressions produced different results"
        );
    }};
}
pub(crate) use assert_eval_eq;

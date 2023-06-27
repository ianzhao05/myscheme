use wasm_bindgen::prelude::wasm_bindgen;

use myscheme::env::Env;
use myscheme::eval_str;
use myscheme::object::ObjectRef;

#[wasm_bindgen]
pub fn eval_expr(s: &str) -> String {
    let env = Env::primitives();
    let res = eval_str(s, env);
    match res {
        Ok(objs) => {
            let mut s = String::new();
            for o in objs {
                match o {
                    ObjectRef::Void => (),
                    _ => s.push_str(&format!("{o:#}\n")),
                }
            }
            s
        }
        Err(e) => format!("{e}"),
    }
}

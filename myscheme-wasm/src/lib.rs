use wasm_bindgen::prelude::wasm_bindgen;

use myscheme::env::primitive_env;
use myscheme::err::SchemeError;
use myscheme::eval_str;
use myscheme::object::ObjectRef;

#[cfg(target_arch = "wasm32")]
use myscheme::port::web::OutBuffer;

#[wasm_bindgen]
pub struct StringResult {
    result: String,
    #[cfg(target_arch = "wasm32")]
    stdout: String,
}

impl StringResult {
    pub fn new(res: Result<Vec<ObjectRef>, SchemeError>) -> Self {
        let result = match res {
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
            Err(e) => format!("{e:#}"),
        };
        Self {
            result,
            #[cfg(target_arch = "wasm32")]
            stdout: OutBuffer::take_buf(),
        }
    }
}

#[wasm_bindgen]
impl StringResult {
    #[wasm_bindgen(getter)]
    pub fn result(&self) -> String {
        self.result.clone()
    }

    #[cfg(target_arch = "wasm32")]
    #[wasm_bindgen(getter)]
    pub fn stdout(&self) -> String {
        self.stdout.clone()
    }
}

#[wasm_bindgen]
pub fn eval_expr(s: &str) -> StringResult {
    StringResult::new(eval_str(s, &primitive_env()))
}

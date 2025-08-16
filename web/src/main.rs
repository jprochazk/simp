use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(code: &str) -> String {
    match simp::parser::parse(code) {
        Ok(ast) => format!("{ast}"),
        Err(err) => err.render(code).to_string(),
    }
}

fn main() {}

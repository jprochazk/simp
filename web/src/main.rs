use std::fmt::Write as _;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(code: &str) -> String {
    match simp::parser::parse(code) {
        Ok(ast) => {
            // we want slightly smaller snapshots, so debug-fmt the contents directly
            let mut o = String::new();
            for stmt in &ast.body {
                writeln!(&mut o, "{stmt:#?}").ok();
            }
            if let Some(tail) = &ast.tail {
                writeln!(&mut o, "{tail:#?}").ok();
            }
            o
        }
        Err(err) => err.render(code).to_string(),
    }
}

fn main() {}

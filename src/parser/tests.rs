use std::fmt::Write as _;
use std::path::Path;

fn parse(code: &str) -> String {
    match super::parse(code) {
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
        Err(err) => format!("{}", err.render(code)),
    }
}

#[glob_test::glob("./inputs/**/*.simp")]
fn test_parse(path: &Path) {
    let input = std::fs::read_to_string(path).unwrap();
    insta::assert_snapshot!(parse(&input));
}

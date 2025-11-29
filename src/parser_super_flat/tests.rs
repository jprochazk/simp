use std::fmt::Write as _;
use std::path::Path;

fn parse(code: &str) -> String {
    match super::parse(code) {
        Ok(ast) => {
            // we want slightly smaller snapshots, so debug-fmt the contents directly
            let mut o = String::new();
            for stmt in ast.root().body(&ast) {
                writeln!(&mut o, "{:#?}", stmt.debug(&ast)).ok();
            }
            if let Some(tail) = ast.root().tail(&ast).into_option() {
                writeln!(&mut o, "{:#?}", tail.debug(&ast)).ok();
            }
            o
        }
        Err(err) => format!("{}", err.render(code)),
    }
}

#[glob_test::glob("../../tests/inputs/**/*.simp")]
fn test_parse(path: &Path) {
    let input = std::fs::read_to_string(path).unwrap();
    insta::assert_snapshot!(parse(&input));
}

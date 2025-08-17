fn parse(code: &str) -> String {
    match super::parse(code) {
        Ok(ast) => format!("{ast}"),
        Err(err) => format!("{}", err.render(code)),
    }
}

#[glob_test::glob("./inputs/**/*.simp")]
fn test_parse(path: &std::path::Path) {
    let input = std::fs::read_to_string(path).unwrap();
    insta::assert_snapshot!(parse(&input));
}

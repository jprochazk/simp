fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let args = args.iter().map(|arg| arg.as_str()).collect::<Vec<_>>();

    match &args[..] {
        ["parser_tree", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let program = simp::parser::parse(&input).expect("failed to parse file");
            println!("{}", program.body.len());
        }
        ["parser_interned", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_interned::parse(&input).expect("failed to parse file");
            println!("{}", ast.program.body.len());
        }
        ["parser_bump", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let bump = bumpalo::Bump::new();
            let ast = simp::parser_bump::parse(&input, &bump).expect("failed to parse file");
            println!("{}", ast.program.body.len());
        }
        ["parser_super_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_super_flat::parse(&input).expect("failed to parse file");
            println!("{}", ast.root().body(&ast).len());
        }
        _ => help(),
    }
}

fn help() {
    println!("simp <parser> <file>");
}

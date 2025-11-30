fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let args = args.iter().map(|arg| arg.as_str()).collect::<Vec<_>>();

    let elapsed = match &args[..] {
        ["parser_tree", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let start = std::time::Instant::now();
            let program = simp::parser::parse(&input).expect("failed to parse file");
            let elapsed = start.elapsed();
            println!("{}", program.body.len());
            elapsed
        }
        ["parser_interned", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let start = std::time::Instant::now();
            let ast = simp::parser_interned::parse(&input).expect("failed to parse file");
            let elapsed = start.elapsed();
            println!("{}", ast.program.body.len());
            elapsed
        }
        ["parser_bump", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let bump = bumpalo::Bump::new();
            let start = std::time::Instant::now();
            let ast = simp::parser_bump::parse(&input, &bump).expect("failed to parse file");
            let elapsed = start.elapsed();
            println!("{}", ast.program.body.len());
            elapsed
        }
        ["parser_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let start = std::time::Instant::now();
            let ast = simp::parser_flat::parse(&input).expect("failed to parse file");
            let elapsed = start.elapsed();
            println!("{}", ast.program.body.len());
            elapsed
        }
        ["parser_super_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let start = std::time::Instant::now();
            let ast = simp::parser_super_flat::parse(&input).expect("failed to parse file");
            let elapsed = start.elapsed();
            println!("{}", ast.root().body(&ast).len());
            elapsed
        }
        _ => {
            help();
            return;
        }
    };

    eprintln!("BENCH_RUNTIME_SEC: {:.9}", elapsed.as_secs_f64());
}

fn help() {
    println!("simp <parser> <file>");
}

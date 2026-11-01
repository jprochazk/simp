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
        ["parser_super_flat_unsafe", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let start = std::time::Instant::now();
            let ast = simp::parser_super_flat_unsafe::parse(&input).expect("failed to parse file");
            let elapsed = start.elapsed();
            println!("{}", ast.root().body(&ast).len());
            elapsed
        }
        ["walk_tree", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let program = simp::parser::parse(&input).expect("failed to parse file");
            bench_walk(|| simp::walk::tree::walk(&program))
        }
        ["walk_interned", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_interned::parse(&input).expect("failed to parse file");
            bench_walk(|| simp::walk::interned::walk(&ast))
        }
        ["walk_bump", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let bump = bumpalo::Bump::new();
            let ast = simp::parser_bump::parse(&input, &bump).expect("failed to parse file");
            bench_walk(|| simp::walk::bump::walk(&ast))
        }
        ["walk_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_flat::parse(&input).expect("failed to parse file");
            bench_walk(|| simp::walk::flat::walk(&ast))
        }
        ["walk_super_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_super_flat::parse(&input).expect("failed to parse file");
            bench_walk(|| simp::walk::super_flat::walk(&ast))
        }
        ["walk_super_flat_unsafe", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_super_flat_unsafe::parse(&input).expect("failed to parse file");
            bench_walk(|| simp::walk::super_flat_unsafe::walk(&ast))
        }
        ["compile_tree", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let program = simp::parser::parse(&input).expect("failed to parse file");
            bench_compile(simp::walk::tree::walk(&program), || {
                simp::compile::tree::compile(&program)
            })
        }
        ["compile_interned", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_interned::parse(&input).expect("failed to parse file");
            bench_compile(simp::walk::interned::walk(&ast), || {
                simp::compile::interned::compile(&ast)
            })
        }
        ["compile_bump", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let bump = bumpalo::Bump::new();
            let ast = simp::parser_bump::parse(&input, &bump).expect("failed to parse file");
            bench_compile(simp::walk::bump::walk(&ast), || {
                simp::compile::bump::compile(&ast)
            })
        }
        ["compile_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_flat::parse(&input).expect("failed to parse file");
            bench_compile(simp::walk::flat::walk(&ast), || {
                simp::compile::flat::compile(&ast)
            })
        }
        ["compile_super_flat", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_super_flat::parse(&input).expect("failed to parse file");
            bench_compile(simp::walk::super_flat::walk(&ast), || {
                simp::compile::super_flat::compile(&ast)
            })
        }
        ["compile_super_flat_unsafe", input] => {
            let input = std::fs::read_to_string(input).expect("failed to read file");
            let ast = simp::parser_super_flat_unsafe::parse(&input).expect("failed to parse file");
            bench_compile(simp::walk::super_flat_unsafe::walk(&ast), || {
                simp::compile::super_flat_unsafe::compile(&ast)
            })
        }
        _ => {
            help();
            return;
        }
    };

    eprintln!("BENCH_RUNTIME_SEC: {:.9}", elapsed.as_secs_f64());
}

fn help() {
    println!("simp <parser_KIND|walk_KIND|compile_KIND> <file>");
    println!("KIND: tree, interned, bump, flat, super_flat, super_flat_unsafe");
}

const BENCH_MIN_ITERS: u32 = 5;
const BENCH_MIN_TOTAL: std::time::Duration = std::time::Duration::from_secs(1);

fn bench_walk(mut walk: impl FnMut() -> simp::walk::Stats) -> std::time::Duration {
    println!("{:?}", walk());
    bench(walk)
}

fn bench_compile(
    stats: simp::walk::Stats,
    mut compile: impl FnMut() -> simp::compile::Program,
) -> std::time::Duration {
    println!("{stats:?}");
    println!("{:?}", compile().summary());
    bench(compile)
}

fn bench<T>(mut f: impl FnMut() -> T) -> std::time::Duration {
    let mut perf = PerfCtl::from_env();

    let mut best = std::time::Duration::MAX;
    let mut total = std::time::Duration::ZERO;
    let mut iters = 0;

    if let Some(perf) = &mut perf {
        perf.send("enable");
    }
    while iters < BENCH_MIN_ITERS || total < BENCH_MIN_TOTAL {
        let start = std::time::Instant::now();
        drop(std::hint::black_box(f()));
        let elapsed = start.elapsed();

        best = best.min(elapsed);
        total += elapsed;
        iters += 1;
    }
    if let Some(perf) = &mut perf {
        perf.send("disable");
    }

    eprintln!("BENCH_ITERS: {iters}");
    eprintln!("BENCH_MEAN_SEC: {:.9}", total.as_secs_f64() / iters as f64);
    best
}

struct PerfCtl {
    ctl: std::fs::File,
    ack: std::fs::File,
}

impl PerfCtl {
    fn from_env() -> Option<Self> {
        let ctl = std::env::var_os("SIMP_PERF_CTL")?;
        let ack = std::env::var_os("SIMP_PERF_ACK")?;
        let ctl = std::fs::OpenOptions::new()
            .write(true)
            .open(ctl)
            .expect("failed to open perf ctl fifo");
        let ack = std::fs::File::open(ack).expect("failed to open perf ack fifo");
        Some(Self { ctl, ack })
    }

    fn send(&mut self, cmd: &str) {
        use std::io::{Read as _, Write as _};

        writeln!(self.ctl, "{cmd}").expect("failed to write perf ctl fifo");
        self.ctl.flush().expect("failed to flush perf ctl fifo");
        let mut buf = [0u8; 16];
        let _ = self
            .ack
            .read(&mut buf)
            .expect("failed to read perf ack fifo");
    }
}

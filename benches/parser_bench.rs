use criterion::{Criterion, Throughput, black_box};

fn parsers(c: &mut Criterion) {
    let input = std::fs::read_to_string("benches/superlarge.simp").unwrap();
    let mut group = c.benchmark_group("parsers");
    group.sample_size(10);
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("parse_tree", |b| {
        b.iter_batched(
            || &input,
            |input| {
                let _ = simp::parser::parse(black_box(input)).unwrap();
            },
            criterion::BatchSize::PerIteration,
        )
    });

    group.bench_function("parse_interned", |b| {
        b.iter_batched(
            || &input,
            |input| {
                let _ = simp::parser_interned::parse(black_box(input)).unwrap();
            },
            criterion::BatchSize::PerIteration,
        )
    });

    group.bench_function("parse_bump", |b| {
        b.iter_batched(
            || (bumpalo::Bump::new(), &input),
            |(bump, input)| {
                let _ = simp::parser_bump::parse(black_box(input), &bump).unwrap();
            },
            criterion::BatchSize::PerIteration,
        )
    });

    group.bench_function("parse_flat", |b| {
        b.iter_batched(
            || &input,
            |input| {
                let _ = simp::parser_flat::parse(black_box(input)).unwrap();
            },
            criterion::BatchSize::PerIteration,
        )
    });

    group.bench_function("parse_super_flat", |b| {
        b.iter_batched(
            || &input,
            |input| {
                let _ = simp::parser_super_flat::parse(black_box(input)).unwrap();
            },
            criterion::BatchSize::PerIteration,
        )
    });

    group.finish();
}

criterion::criterion_group! {
    name = benches;
    config = criterion::Criterion::default();
    targets = parsers
}

fn main() {
    benches();
    criterion::Criterion::default()
        .configure_from_args()
        .final_summary();
}

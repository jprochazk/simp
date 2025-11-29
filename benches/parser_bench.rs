use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

fn parsers(c: &mut Criterion) {
    let input = std::fs::read_to_string("benches/superlarge.simp").unwrap();
    let mut group = c.benchmark_group("parsers");
    group.sample_size(10);
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("parse_tree", |b| {
        b.iter(|| simp::parser::parse(black_box(&input)))
    });

    group.bench_function("parse_super_flat", |b| {
        b.iter(|| simp::parser_super_flat::parse(black_box(&input)))
    });

    group.finish();
}

criterion_group!(benches, parsers);
criterion_main!(benches);

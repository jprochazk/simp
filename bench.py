#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "matplotlib",
# ]
# ///

import csv
import os
import subprocess
import tempfile
from pathlib import Path

import matplotlib.pyplot as plt


KINDS = [
    "parser_tree",
    "parser_interned",
    "parser_bump",
    "parser_super_flat",
]


def run(cmd, **kwargs):
    subprocess.run(cmd, check=True, **kwargs)


def build_bench():
    run(["cargo", "build", "--release"])
    return "target/release/simp"


SNIPPETS = [
    # Simple function definitions
    "fn simple() {}",
    "fn with_params(a, b, c) { a }",
    "fn nested() { fn inner() { 42 } }",

    # Let statements
    "let x = 0;",
    "let y = add(1, 2);",
    "let z = if 1 { 10 } else { 20 };",

    # Function calls
    "f();",
    "g(1, 2, 3);",
    "h(a, b, c, d, e, f);",

    # If expressions
    "if 0 {}",
    "if x { y } else { z }",
    "if a { b } else if c { d } else { e }",

    # Blocks
    "{}",
    "{ 0 }",
    "{ let a = 1; a }",
    "{ { { nested } } }",

    # Binary operations
    "a + b;",
    "x * y + z;",
    "a && b || c;",
    "x == y && z != w;",
    "a < b && c > d;",

    # Complex expressions
    "or || and0 && and0 && and1;",
    "and && eq0 == eq0 == eq1;",
    "eq == ord0 < ord0 < ord1;",
    "ord < add0 + add0 + add1;",
    "add + mul0 * mul0 * mul1;",
    "mul * -unary;",
    "-call();",

    # Functions with bodies
    """fn fibonacci(n) {
    if n < 2 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}""",
    """fn factorial(n) {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}""",
    # Complex nested structures
    """fn complex(a, b, c) {
    let x = a + b;
    let y = x * c;
    if y > 0 {
        let result = compute(y);
        result
    } else {
        -y
    }
}""",
]


def generate_test_file(path, target_bytes):
    """Generate a test file with valid simp code up to target_bytes."""
    content = []
    total_bytes = 0

    while total_bytes < target_bytes:
        for snippet in SNIPPETS:
            content.append(snippet)
            content.append("\n\n")
            total_bytes += len(snippet) + 2

            if total_bytes >= target_bytes:
                break

    full_content = "".join(content)

    with open(path, "w") as f:
        f.write(full_content)

    size = len(full_content)
    if size >= 1024 * 1024:
        size_str = f"{size / (1024 * 1024):.2f} MiB"
    elif size >= 1024:
        size_str = f"{size / 1024:.2f} KiB"
    else:
        size_str = f"{size} bytes"

    print(f"Generated {path} with {size_str}")
    return path


def generate_input_files(output_dir):
    """Generate test files in 10x increments up to 10 MiB."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    KB = 1024
    MB = 1024 * KB

    sizes = [
        ("input_1kb.simp", 1 * KB),
        ("input_10kb.simp", 10 * KB),
        ("input_100kb.simp", 100 * KB),
        ("input_1mb.simp", 1 * MB),
        ("input_10mb.simp", 10 * MB),
        ("input_100mb.simp", 100 * MB),
    ]

    files = []
    for filename, target_bytes in sizes:
        path = output_dir / filename
        generate_test_file(path, target_bytes)
        files.append(str(path))

    print(f"\nGenerated {len(files)} test files in {output_dir}")
    return files


def run_job(binary, kind, input_file):
    """
    Run one job:

        /usr/bin/time -f "%e %M %F %R" <binary> <kind> <input_file>

    Returns a dict with metrics.
    """
    if not os.path.exists(input_file):
        raise SystemExit(f"Input file does not exist: {input_file}")

    input_size_bytes = os.path.getsize(input_file)

    # Adjust this path if your `time` binary is elsewhere.
    time_prog = "/usr/bin/time"

    with tempfile.NamedTemporaryFile(mode="r+") as tf:
        time_cmd = [
            time_prog,
            "-f",
            "%e %M %F %R",  # runtime(s), maxRSS(kb), majPF, minPF
            "-o",
            tf.name,
            binary,
            kind,
            input_file,
        ]

        run(time_cmd)

        tf.seek(0)
        line = tf.read().strip()
        parts = line.split()
        if len(parts) != 4:
            raise RuntimeError(
                f"Unexpected /usr/bin/time output format: {line!r}"
            )

        runtime_sec = float(parts[0])
        max_rss_kb = int(parts[1])
        maj_pf = int(parts[2])
        min_pf = int(parts[3])

    return {
        "kind": kind,
        "input_file": input_file,
        "input_size_bytes": input_size_bytes,
        "runtime_sec": runtime_sec,
        "max_rss_kb": max_rss_kb,
        "maj_pf": maj_pf,
        "min_pf": min_pf,
    }


def enrich_results(results):
    for r in results:
        r["input_size_mib"] = r["input_size_bytes"] / (1024 * 1024)
        r["max_rss_mib"] = r["max_rss_kb"] / 1024.0
        r["pf_total"] = r["maj_pf"] + r["min_pf"]


def plot_results(results, output_path):
    if not results:
        print("No results to plot.")
        return

    enrich_results(results)

    max_rss = max(r["max_rss_mib"] for r in results)
    min_rss = min(r["max_rss_mib"] for r in results)
    rss_span = max(max_rss - min_rss, 1e-6)

    kinds = sorted(set(r["kind"] for r in results))

    plt.figure(figsize=(10, 7))

    for kind in kinds:
        xs = []
        ys = []
        sizes = []
        labels = []

        for r in results:
            if r["kind"] != kind:
                continue
            xs.append(r["runtime_sec"])
            ys.append(r["input_size_mib"])

            # Map [min_rss, max_rss] -> [50, 500] for marker area
            norm = (r["max_rss_mib"] - min_rss) / rss_span
            sizes.append(50 + 450 * norm)
            labels.append(str(r["pf_total"]))

        scatter = plt.scatter(xs, ys, s=sizes, alpha=0.7, label=kind)

        # Annotate with total page faults
        for x, y, label in zip(xs, ys, labels):
            plt.annotate(
                label,
                (x, y),
                textcoords="offset points",
                xytext=(3, 3),
                fontsize=8,
                alpha=0.7,
            )

    plt.xlabel("Runtime (s)")
    plt.ylabel("Input size (MiB)")
    plt.title(
        "Bench profile: color = kind, marker size = max RSS (MiB), "
        "label = total page faults"
    )
    plt.legend(title="kind")
    plt.grid(True, linestyle="--", alpha=0.3)

    plt.tight_layout()
    plt.savefig(output_path, dpi=200)
    print(f"Wrote plot to {output_path}")


def write_csv(results, path):
    if not results:
        return

    enrich_results(results)

    fieldnames = [
        "kind",
        "input_file",
        "input_size_bytes",
        "input_size_mib",
        "runtime_sec",
        "max_rss_kb",
        "max_rss_mib",
        "maj_pf",
        "min_pf",
        "pf_total",
    ]
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for r in results:
            writer.writerow({k: r.get(k) for k in fieldnames})
    print(f"Wrote CSV to {path}")


def main():
    output_dir = "benches/inputs"
    input_files = generate_input_files(output_dir)
    binary_path = build_bench()

    results = []
    for kind in KINDS:
        for input_file in input_files:
            print(f"\n=== Running parser={kind} input={input_file} ===")
            res = run_job(binary_path, kind, input_file)
            results.append(res)
            print(
                f"  runtime={res['runtime_sec']:.3f}s, "
                f"max_rss={res['max_rss_kb']} KB, "
                f"maj_pf={res['maj_pf']}, min_pf={res['min_pf']}"
            )

    plot_path = "bench.png"
    csv_path = "bench.csv"
    plot_results(results, plot_path)
    write_csv(results, csv_path)


if __name__ == "__main__":
    main()


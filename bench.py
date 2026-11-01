#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "matplotlib",
# ]
# ///

import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import matplotlib.pyplot as plt


KINDS = [
    "parser_tree",
    "parser_interned",
    "parser_flat",
    "parser_bump",
    "parser_super_flat",
    "parser_super_flat_unsafe",
]

WALK_KINDS = [
    "walk_tree",
    "walk_interned",
    "walk_flat",
    "walk_bump",
    "walk_super_flat",
    "walk_super_flat_unsafe",
]

COMPILE_KINDS = [kind.replace("walk_", "compile_") for kind in WALK_KINDS]

MODES = {
    "parse": (KINDS, "bench", "Parser"),
    "walk": (WALK_KINDS, "bench_walk", "Walk"),
    "compile": (COMPILE_KINDS, "bench_compile", "Compile"),
}

HAVE_PERF = sys.platform == "linux" and shutil.which("perf") is not None

PERF_EVENTS = [
    "cycles:u",
    "instructions:u",
    "L1-dcache-load-misses:u",
    "cache-misses:u",
]


def run(cmd, **kwargs):
    subprocess.run(cmd, check=True, **kwargs)


def build_bench():
    run(["cargo", "build", "--release"])
    return "target/release/simp"


SNIPPETS = [
    "fn simple() {}",
    "fn with_params(a, b, c) { a }",
    "fn nested() { fn inner() { 42 } }",
    "let x = 0;",
    "let y = add(1, 2);",
    "let z = if 1 { 10 } else { 20 };",
    "f();",
    "g(1, 2, 3);",
    "h(a, b, c, d, e, f);",
    "if 0 {}",
    "if x { y } else { z }",
    "if a { b } else if c { d } else { e }",
    "{}",
    "{ 0 }",
    "{ let a = 1; a }",
    "{ { { nested } } }",
    "a + b;",
    "x * y + z;",
    "a && b || c;",
    "x == y && z != w;",
    "a < b && c > d;",
    "or || and0 && and0 && and1;",
    "and && eq0 == eq0 == eq1;",
    "eq == ord0 < ord0 < ord1;",
    "ord < add0 + add0 + add1;",
    "add + mul0 * mul0 * mul1;",
    "mul * -unary;",
    "-call();",
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
    """Generate test files with smooth progression up to 100 MiB."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    KB = 1024
    MB = 1024 * KB

    sizes = [
        ("input_1kb.simp", 1 * KB),
        ("input_10kb.simp", 10 * KB),
        ("input_100kb.simp", 100 * KB),
        ("input_250kb.simp", 250 * KB),
        ("input_500kb.simp", 500 * KB),
        ("input_1mb.simp", 1 * MB),
        ("input_2500kb.simp", int(2.5 * MB)),
        ("input_5mb.simp", 5 * MB),
        ("input_10mb.simp", 10 * MB),
        ("input_25mb.simp", 25 * MB),
        ("input_50mb.simp", 50 * MB),
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

        /usr/bin/time -f "%M %F %R" <binary> <kind> <input_file>

    Returns a dict with metrics.
    Program reports its own runtime via stderr.
    """
    if not os.path.exists(input_file):
        raise SystemExit(f"Input file does not exist: {input_file}")

    input_size_bytes = os.path.getsize(input_file)

    # Count lines in the input file
    with open(input_file, 'r') as f:
        line_count = sum(1 for _ in f)

    # Adjust this path if your `time` binary is elsewhere.
    time_prog = "/usr/bin/time"

    # `/usr/bin/time` makes it easy to report memory stats,
    # but it has poor runtime precision.
    # We also don't care about "process overhead", so we
    # have the program measure and report its own runtime.
    if sys.platform == "darwin":
        result, runtime_sec, max_rss_kb, maj_pf, min_pf = measure_darwin(
            time_prog, binary, kind, input_file
        )
    else:
        result, runtime_sec, max_rss_kb, maj_pf, min_pf = measure_linux(
            time_prog, binary, kind, input_file
        )

    res = {
        "kind": kind,
        "input_file": input_file,
        "input_size_bytes": input_size_bytes,
        "line_count": line_count,
        "runtime_sec": runtime_sec,
        "max_rss_kb": max_rss_kb,
        "maj_pf": maj_pf,
        "min_pf": min_pf,
    }

    if kind.startswith(("walk_", "compile_")):
        stmts = int(re.search(r"stmts: (\d+)", result.stdout).group(1))
        exprs = int(re.search(r"exprs: (\d+)", result.stdout).group(1))
        res["nodes"] = stmts + exprs

    return res


def measure_darwin(time_prog, binary, kind, input_file):
    result = subprocess.run(
        [time_prog, "-l", binary, kind, input_file],
        check=True, capture_output=True, text=True,
    )

    def stat(pattern):
        match = re.search(pattern, result.stderr)
        if match is None:
            raise RuntimeError(f"Failed to parse {pattern!r} from program output")
        return match.group(1)

    runtime_sec = float(stat(r"BENCH_RUNTIME_SEC: ([\d.]+)"))
    max_rss_kb = int(stat(r"(\d+)\s+maximum resident set size")) // 1024
    maj_pf = int(stat(r"(\d+)\s+page faults"))
    min_pf = int(stat(r"(\d+)\s+page reclaims"))
    return result, runtime_sec, max_rss_kb, maj_pf, min_pf


def measure_linux(time_prog, binary, kind, input_file):
    with tempfile.NamedTemporaryFile(mode="r+") as tf:
        time_cmd = [
            "taskset", "-c", "0",
            time_prog,
            "-f",
            "%M %F %R",  # maxRSS(kb), majPF, minPF
            "-o",
            tf.name,
            binary,
            kind,
            input_file,
        ]

        # Capture stderr to get the program's runtime measurement
        result = subprocess.run(time_cmd, check=True, capture_output=True, text=True)

        # Parse runtime from stderr
        runtime_sec = None
        for line in result.stderr.splitlines():
            if line.startswith("BENCH_RUNTIME_SEC:"):
                runtime_sec = float(line.split(":", 1)[1].strip())
                break

        if runtime_sec is None:
            raise RuntimeError("Failed to parse BENCH_RUNTIME_SEC from program output")

        # Read memory and page fault stats from /usr/bin/time
        tf.seek(0)
        line = tf.read().strip()
        parts = line.split()
        if len(parts) != 3:
            raise RuntimeError(
                f"Unexpected /usr/bin/time output format: {line!r}"
            )

        max_rss_kb = int(parts[0])
        maj_pf = int(parts[1])
        min_pf = int(parts[2])

    return result, runtime_sec, max_rss_kb, maj_pf, min_pf


def run_perf(binary, kind, input_file):
    with tempfile.TemporaryDirectory() as tmp:
        ctl = os.path.join(tmp, "ctl")
        ack = os.path.join(tmp, "ack")
        out = os.path.join(tmp, "perf.csv")
        os.mkfifo(ctl)
        os.mkfifo(ack)

        cmd = [
            "perf", "stat",
            "-x", ",",
            "-o", out,
            "--delay=-1",
            f"--control=fifo:{ctl},{ack}",
            "-e", ",".join(PERF_EVENTS),
            "taskset", "-c", "0",
            binary, kind, input_file,
        ]
        env = dict(os.environ, SIMP_PERF_CTL=ctl, SIMP_PERF_ACK=ack)
        result = subprocess.run(cmd, check=True, capture_output=True, text=True, env=env)

        iters = int(re.search(r"BENCH_ITERS: (\d+)", result.stderr).group(1))

        counters = {}
        with open(out) as f:
            for line in f:
                parts = line.strip().split(",")
                if len(parts) < 3 or parts[2] not in PERF_EVENTS:
                    continue
                if not parts[0] or not parts[0][0].isdigit():
                    raise RuntimeError(f"perf counter not available: {line!r}")
                counters[parts[2]] = float(parts[0]) / iters

    return counters


def enrich_results(results):
    for r in results:
        r["input_size_mib"] = r["input_size_bytes"] / (1024 * 1024)
        r["max_rss_mib"] = r["max_rss_kb"] / 1024.0
        r["pf_total"] = r["maj_pf"] + r["min_pf"]
        r["loc_per_s"] = r["line_count"] / r["runtime_sec"] if r["runtime_sec"] > 0 else 0
        r["mloc_per_s"] = r["loc_per_s"] / 1_000_000


def plot_results(results, output_prefix, label="Parser"):
    if not results:
        print("No results to plot.")
        return

    enrich_results(results)
    kinds = sorted(set(r["kind"] for r in results))

    # Plot 1: Runtime vs Input Size
    plt.figure(figsize=(10, 7))
    for kind in kinds:
        xs = []
        ys = []

        for r in results:
            if r["kind"] != kind:
                continue
            xs.append(r["input_size_mib"])
            ys.append(r["runtime_sec"])

        sorted_data = sorted(zip(xs, ys))
        xs, ys = zip(*sorted_data) if sorted_data else ([], [])
        plt.plot(xs, ys, marker='o', label=kind, linewidth=2, markersize=6)

    plt.xlabel("Input size (MiB)")
    plt.ylabel("Runtime (s)")
    plt.title(f"{label} Performance: Runtime vs Input Size")
    plt.legend(title=label)
    plt.grid(True, linestyle="--", alpha=0.3)
    plt.tight_layout()
    runtime_path = f"{output_prefix}_runtime.png"
    plt.savefig(runtime_path, dpi=200)
    print(f"Wrote runtime plot to {runtime_path}")
    plt.close()

    # Plot 2: Max RSS vs Input Size
    plt.figure(figsize=(10, 7))
    for kind in kinds:
        xs = []
        ys = []

        for r in results:
            if r["kind"] != kind:
                continue
            xs.append(r["input_size_mib"])
            ys.append(r["max_rss_mib"])

        sorted_data = sorted(zip(xs, ys))
        xs, ys = zip(*sorted_data) if sorted_data else ([], [])
        plt.plot(xs, ys, marker='o', label=kind, linewidth=2, markersize=6)

    plt.xlabel("Input size (MiB)")
    plt.ylabel("Max RSS (MiB)")
    plt.title(f"{label} Memory Usage: Max RSS vs Input Size")
    plt.legend(title=label)
    plt.grid(True, linestyle="--", alpha=0.3)
    plt.tight_layout()
    rss_path = f"{output_prefix}_rss.png"
    plt.savefig(rss_path, dpi=200)
    print(f"Wrote RSS plot to {rss_path}")
    plt.close()

    # Plot 3: Page Faults vs Input Size
    plt.figure(figsize=(10, 7))
    for kind in kinds:
        xs = []
        ys = []

        for r in results:
            if r["kind"] != kind:
                continue
            xs.append(r["input_size_mib"])
            ys.append(r["pf_total"])

        sorted_data = sorted(zip(xs, ys))
        xs, ys = zip(*sorted_data) if sorted_data else ([], [])
        plt.plot(xs, ys, marker='o', label=kind, linewidth=2, markersize=6)

    plt.xlabel("Input size (MiB)")
    plt.ylabel("Total Page Faults")
    plt.title(f"{label} Page Faults: Total vs Input Size")
    plt.legend(title=label)
    plt.grid(True, linestyle="--", alpha=0.3)
    plt.tight_layout()
    pf_path = f"{output_prefix}_pagefaults.png"
    plt.savefig(pf_path, dpi=200)
    print(f"Wrote page faults plot to {pf_path}")
    plt.close()

    # Plot 4: Million Lines Per Second vs Input Size
    plt.figure(figsize=(10, 7))
    for kind in kinds:
        xs = []
        ys = []

        for r in results:
            if r["kind"] != kind:
                continue
            xs.append(r["input_size_mib"])
            ys.append(r["mloc_per_s"])

        sorted_data = sorted(zip(xs, ys))
        xs, ys = zip(*sorted_data) if sorted_data else ([], [])
        plt.plot(xs, ys, marker='o', label=kind, linewidth=2, markersize=6)

    plt.xlabel("Input size (MiB)")
    plt.ylabel("MLoC/s")
    plt.title(f"{label} Throughput: MLoC/s vs Input Size")
    plt.xscale('log')
    plt.legend(title=label)
    plt.grid(True, linestyle="--", alpha=0.3)
    plt.tight_layout()
    lps_path = f"{output_prefix}_loc.png"
    plt.savefig(lps_path, dpi=200)
    print(f"Wrote mloc/s plot to {lps_path}")
    plt.close()


def plot_metric(results, kinds, key, ylabel, title, path, label):
    plt.figure(figsize=(10, 7))
    for kind in kinds:
        data = sorted(
            (r["input_size_mib"], r[key])
            for r in results
            if r["kind"] == kind and key in r
        )
        if not data:
            continue
        xs, ys = zip(*data)
        plt.plot(xs, ys, marker='o', label=kind, linewidth=2, markersize=6)

    plt.xlabel("Input size (MiB)")
    plt.ylabel(ylabel)
    plt.title(title)
    plt.xscale('log')
    plt.legend(title=label)
    plt.grid(True, linestyle="--", alpha=0.3)
    plt.tight_layout()
    plt.savefig(path, dpi=200)
    print(f"Wrote {key} plot to {path}")
    plt.close()


def plot_per_node_results(results, output_prefix, label):
    for r in results:
        nodes = r["nodes"]
        r["ns_per_node"] = r["runtime_sec"] * 1e9 / nodes
        for event in PERF_EVENTS:
            if event in r:
                r[f"{event}_per_node"] = r[event] / nodes

    kinds = sorted(set(r["kind"] for r in results))
    plot_metric(
        results, kinds, "ns_per_node", "ns / node",
        f"{label}: time per node vs Input Size",
        f"{output_prefix}_ns_per_node.png",
        label,
    )
    for event in PERF_EVENTS:
        if not any(event in r for r in results):
            continue
        name = event.removesuffix(":u")
        plot_metric(
            results, kinds, f"{event}_per_node", f"{name} / node",
            f"{label}: {name} per node vs Input Size",
            f"{output_prefix}_{name}_per_node.png",
            label,
        )


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "parse"
    if mode not in MODES:
        raise SystemExit(f"usage: {sys.argv[0]} [{'|'.join(MODES)}]")
    kinds, output_prefix, label = MODES[mode]

    output_dir = "benches/inputs"
    input_files = generate_input_files(output_dir)
    binary_path = build_bench()

    results = []
    for kind in kinds:
        for input_file in input_files:
            print(f"\n=== Running parser={kind} input={input_file} ===")
            res = run_job(binary_path, kind, input_file)
            results.append(res)
            runtime_ms = res['runtime_sec'] * 1000
            loc_per_s = res['line_count'] / res['runtime_sec'] if res['runtime_sec'] > 0 else 0
            print(
                f"  lines={res['line_count']}, "
                f"runtime={res['runtime_sec']:.3f}s ({runtime_ms:.2f}ms), "
                f"loc/s={loc_per_s:.2f}, "
                f"max_rss={res['max_rss_kb']} KB, "
                f"maj_pf={res['maj_pf']}, min_pf={res['min_pf']}"
            )
            if mode != "parse":
                if HAVE_PERF:
                    res.update(run_perf(binary_path, kind, input_file))
                print(
                    f"  ns/node={runtime_ms * 1e6 / res['nodes']:.3f}, "
                    + ", ".join(
                        f"{e.removesuffix(':u')}/node={res[e] / res['nodes']:.3f}"
                        for e in PERF_EVENTS
                        if e in res
                    )
                )

    with open(f"{output_prefix}_results.json", "w") as f:
        json.dump(results, f, indent=1)

    plot_results(results, output_prefix, label=label)
    if mode != "parse":
        plot_per_node_results(results, output_prefix, label)


if __name__ == "__main__":
    main()


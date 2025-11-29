#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "matplotlib",
# ]
# ///

import argparse
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
    print("+", " ".join(cmd))
    subprocess.run(cmd, check=True, **kwargs)


def build_bench(bin_name):
    run(["cargo", "build", "--profile", "bench"])

    path = os.path.join("target", "bench", "simp")
    return path


def collect_input_files(input_dir, pattern):
    root = Path(input_dir)
    if not root.is_dir():
        raise SystemExit(f"--input-dir is not a directory: {input_dir}")

    files = sorted(
        str(p) for p in root.glob(pattern) if p.is_file()
    )
    if not files:
        raise SystemExit(
            f"No input files found in {input_dir!r} with pattern {pattern!r}"
        )

    print(f"Found {len(files)} input file(s):")
    for f in files:
        print("  -", f)
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
    parser = argparse.ArgumentParser(
        description=(
            "measure max RSS/runtime/page faults, and plot results"
        )
    )
    parser.add_argument(
        "--input-dir",
        required=True,
        help="Directory containing input files.",
    )
    parser.add_argument(
        "--pattern",
        default="*",
        help="Glob pattern within --input-dir (default: '*').",
    )
    parser.add_argument(
        "--output-prefix",
        default="bench_profile",
        help="Prefix for CSV and PNG output files (default: bench_profile).",
    )

    args = parser.parse_args()

    binary_path = build_bench()
    input_files = collect_input_files(args.input_dir, args.pattern)

    print("\nKINDS:", ", ".join(KINDS))

    results = []
    for kind in KINDS:
        for input_file in input_files:
            print(f"\n=== Running kind={kind} input={input_file} ===")
            res = run_job(binary_path, kind, input_file)
            results.append(res)
            print(
                f"  runtime={res['runtime_sec']:.3f}s, "
                f"max_rss={res['max_rss_kb']} KB, "
                f"maj_pf={res['maj_pf']}, min_pf={res['min_pf']}"
            )

    plot_path = f"{args.output_prefix}.png"
    csv_path = f"{args.output_prefix}.csv"
    plot_results(results, plot_path)
    write_csv(results, csv_path)


if __name__ == "__main__":
    main()


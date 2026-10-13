"""Render source-pinned terminal media from logs produced by the real R workflow."""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
import textwrap
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "docs" / "demo"
MEDIA = ("145-cli-guard.gif", "145-cli-guard.mp4", "145-cli-guard-poster.png")
SOURCE_PATHS = (
    ".github/workflows/regime-core.yml",
    "README.md",
    "run_regime_core.R",
    "runner_options.R",
    "test_runner_options.R",
    "test_temporal.R",
    "scripts/render_cli_demo.py",
    "scripts/verify_cli_demo.py",
)


def git(*args: str) -> str:
    return subprocess.check_output(["git", *args], cwd=ROOT, text=True).strip()


def selected(text: str, needles: tuple[str, ...]) -> list[str]:
    return [line.strip() for line in text.splitlines() if any(needle in line for needle in needles)]


def wrapped(lines: list[str], width: int = 82) -> str:
    rendered: list[str] = []
    for line in lines:
        rendered.extend(textwrap.wrap(line, width=width, replace_whitespace=False) or [""])
    return "\n".join(rendered[:18])


def run_quiet(args: list[str]) -> None:
    result = subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    if result.returncode:
        raise RuntimeError(f"Command failed ({result.returncode}): {' '.join(args)}\n{result.stdout}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--logs", type=Path, required=True)
    args = parser.parse_args()
    logs = {name: (args.logs / f"{name}.log").read_text(errors="replace") for name in (
        "runner-options", "temporal", "archived", "past-only"
    )}

    required = {
        "runner-options": ("--past-onyl", "Usage: Rscript run_regime_core.R [--past-only]", "checks passed"),
        "temporal": ("Past-only alignment and future perturbation checks passed",),
        "archived": ("Runner mode: archived retrospective reproduction", "ms_model2", "ms_model3"),
        "past-only": ("Runner mode: past-only re-estimation", "past_model1", "past_model4"),
    }
    for name, needles in required.items():
        for needle in needles:
            if needle not in logs[name]:
                raise RuntimeError(f"Missing {needle!r} in {name}.log")

    source = git("rev-parse", "HEAD")
    dirty = git("status", "--porcelain", "--untracked-files=no")
    if dirty:
        raise RuntimeError(f"Refusing to render from dirty tracked source:\n{dirty}")

    slides = [
        [
            "145-Project- / strict runner CLI",
            "",
            f"source {source}",
            "",
            "Actual GitHub Actions run; committed research data; real R/MSwM fits.",
            "The demo verifies routing and reproducible execution only.",
            "It does not claim convergence, forecasting, trading, or profitability.",
        ],
        [
            "$ Rscript test_runner_options.R",
            "",
            *selected(logs["runner-options"], (
                "Observed typo rejection", "Unknown or duplicate", "Usage:", "checks passed"
            )),
            "",
            "Result: --past-onyl exits non-zero before packages or models load.",
        ],
        [
            "$ Rscript test_temporal.R",
            *selected(logs["temporal"], ("checks passed",)),
            "",
            "$ Rscript run_regime_core.R",
            *selected(logs["archived"], ("Runner mode:", "Fitting original:", "ms_model2", "ms_model3")),
            "",
            "Archived mode remains an explicit retrospective reproduction.",
        ],
        [
            "$ Rscript run_regime_core.R --past-only",
            *selected(logs["past-only"], ("Runner mode:", "Fitting corrected", "past_model")),
            "",
            "The past-only lane uses chronological predictors from earlier observations.",
        ],
        [
            "Verified workflow result",
            "",
            "PASS  strict parser unit checks",
            "PASS  real-runner typo rejection",
            "PASS  temporal alignment checks",
            "PASS  archived and past-only integration fits",
            "PASS  probability, transition, and model round-trip checks",
            "",
            "Generated from the exact source commit recorded in capture.json.",
        ],
    ]

    OUTPUT.mkdir(parents=True, exist_ok=True)
    font_candidates = (
        Path("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"),
        Path("/usr/share/fonts/dejavu/DejaVuSansMono.ttf"),
    )
    font = next((path for path in font_candidates if path.exists()), None)
    if font is None:
        raise RuntimeError("DejaVu Sans Mono is required to render the demo")

    with tempfile.TemporaryDirectory(prefix="145-cli-demo-") as temporary:
        work = Path(temporary)
        frames: list[Path] = []
        for index, lines in enumerate(slides):
            text_path = work / f"slide-{index:02d}.txt"
            frame_path = work / f"frame-{index:02d}.png"
            text_path.write_text(wrapped(lines) + "\n")
            draw = (
                f"drawtext=fontfile='{font}':textfile='{text_path}':"
                "fontcolor=0xd7e7d9:fontsize=27:x=56:y=52:line_spacing=14:expansion=none"
            )
            run_quiet([
                "ffmpeg", "-y", "-f", "lavfi", "-i", "color=c=0x0b1210:s=1280x720:d=1",
                "-vf", draw, "-frames:v", "1", str(frame_path),
            ])
            frames.append(frame_path)

        concat = work / "frames.txt"
        entries: list[str] = []
        durations = (3.0, 4.0, 4.0, 5.0, 4.0)
        for frame, duration in zip(frames, durations):
            entries.extend((f"file '{frame}'", f"duration {duration}"))
        entries.append(f"file '{frames[-1]}'")
        concat.write_text("\n".join(entries) + "\n")

        mp4 = OUTPUT / "145-cli-guard.mp4"
        gif = OUTPUT / "145-cli-guard.gif"
        poster = OUTPUT / "145-cli-guard-poster.png"
        run_quiet([
            "ffmpeg", "-y", "-f", "concat", "-safe", "0", "-i", str(concat),
            "-vf", "fps=25,format=yuv420p", "-c:v", "libx264", "-movflags", "+faststart", str(mp4),
        ])
        palette = work / "palette.png"
        run_quiet([
            "ffmpeg", "-y", "-i", str(mp4),
            "-vf", "fps=10,scale=960:-2:flags=lanczos,palettegen=max_colors=128:stats_mode=diff", str(palette),
        ])
        run_quiet([
            "ffmpeg", "-y", "-i", str(mp4), "-i", str(palette),
            "-lavfi", "fps=10,scale=960:-2:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=4",
            str(gif),
        ])
        shutil.copyfile(frames[-1], poster)

    source_blobs = {path: git("rev-parse", f"{source}:{path}") for path in SOURCE_PATHS}
    artifacts = {}
    for name in MEDIA:
        data = (OUTPUT / name).read_bytes()
        artifacts[name] = {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()}
    receipt = {
        "schema": "145-cli-demo/v1",
        "scope": "Strict CLI routing and real archived/past-only execution; no forecasting or trading claim.",
        "source_commit": source,
        "source_blobs": source_blobs,
        "commands": [
            "Rscript test_runner_options.R",
            "Rscript test_temporal.R",
            "Rscript run_regime_core.R",
            "Rscript run_regime_core.R --past-only",
        ],
        "artifacts": artifacts,
    }
    (OUTPUT / "capture.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(f"Rendered strict-CLI demo from {source}")


if __name__ == "__main__":
    main()

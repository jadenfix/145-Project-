"""Verify published CLI media, source provenance, and complete media decoding."""
from __future__ import annotations

import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEMO = ROOT / "docs" / "demo"
EXPECTED_ARTIFACTS = {
    "145-cli-guard.gif",
    "145-cli-guard.mp4",
    "145-cli-guard-poster.png",
}


def output(*args: str) -> str:
    return subprocess.check_output(args, cwd=ROOT, text=True).strip()


capture = json.loads((DEMO / "capture.json").read_text())
assert capture["schema"] == "145-cli-demo/v1"
assert set(capture["artifacts"]) == EXPECTED_ARTIFACTS
source = capture["source_commit"]
subprocess.run(["git", "merge-base", "--is-ancestor", source, "HEAD"], cwd=ROOT, check=True)

for path, expected in capture["source_blobs"].items():
    recorded = output("git", "rev-parse", f"{source}:{path}")
    current = output("git", "rev-parse", f"HEAD:{path}")
    assert recorded == expected, (path, recorded, expected)
    assert current == expected, (path, current, expected)

for name, expected in capture["artifacts"].items():
    data = (DEMO / name).read_bytes()
    assert len(data) == expected["bytes"], name
    assert hashlib.sha256(data).hexdigest() == expected["sha256"], name

for name in ("145-cli-guard.gif", "145-cli-guard.mp4"):
    subprocess.run(
        ["ffmpeg", "-v", "error", "-i", str(DEMO / name), "-f", "null", "-"],
        check=True,
    )

print(f"Verified strict-CLI demo and source {source}")

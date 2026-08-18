#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$ROOT/scripts/generate_mir_v0_riscv_linux_bridge_policy.py" --check
printf '%s\n' 'MIR-v0 RISC-V Linux bridge policy freshness: clean'

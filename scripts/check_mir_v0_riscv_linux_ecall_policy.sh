#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$ROOT/scripts/generate_mir_v0_riscv_linux_ecall_policy.py" --check
printf '%s\n' 'MIR-v0 RISC-V Linux ecall policy freshness: clean'

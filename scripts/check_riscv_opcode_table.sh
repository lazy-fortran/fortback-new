#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$ROOT/scripts/generate_riscv_opcode_table.py" --check
printf '%s\n' 'RISC-V opcode table freshness: clean'

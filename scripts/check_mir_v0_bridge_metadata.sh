#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$ROOT/scripts/generate_mir_v0_bridge_metadata.py" --check
printf '%s\n' 'MIR-v0 bridge metadata freshness: clean'

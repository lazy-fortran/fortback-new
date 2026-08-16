#!/usr/bin/env bash
set -euo pipefail

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/fortback-mir-v0.XXXXXX")
trap 'rm -rf "$work_dir"' EXIT

input_file="$work_dir/mir-v0.sx"
output_file="$work_dir/output.elf"

printf '%s' '(mir-function (name main) (entry-block 0) (instruction-count 2) (instructions (instruction (id 0) (opcode add) (source-rule frontend-v0/program) (result (id 1) (kind integer) (type i32))) (instruction (id 1) (opcode return) (source-rule frontend-v0/program) (result (id 1) (kind integer) (type i32)))))' >"$input_file"

fo exec fortback-mir-v0 "$input_file" "$output_file"
test -x "$output_file"
qemu-riscv64 "$output_file"

echo 'MIR-v0 CLI trailing-buffer regression: ok'

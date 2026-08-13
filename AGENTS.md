# fortback-new

This repository is the production target-specific backend for Lazy Fortran.
It owns TargetIR, target specifications, generated backend source, and
behavioral tests. The laboratory in `../lazy-fortran-new` owns the roadmap,
decisions, experiments, runs, provenance, external-source manifests, and
cross-repository wiring.

Keep this repository production-only. Do not add a local `ROADMAP.md`, research
notes, experiment manifests, model logs, orchestration, or copied external ISA
payloads. Read the laboratory's central roadmap and accepted decisions when a
phase gate or architectural question is involved.

## Scope

- `src/` — TargetIR and generated backend library code.
- `test/` — behavioral tests with independently established expected values.
- `scripts/` — repository quality gates.
- `.github/workflows/` — reproducible build and test gates.

Target descriptions are derived from pinned machine-readable specifications;
they are not hand-transcribed into production code. Keep MIR and
MIR-dependent instruction selection in `ffc-new`. Do not implement handwritten
encoders, decoders, object writers, or ISA ingestion as a scaffold substitute.

## Build and test

Run the complete local gate before committing:

```sh
fo
fo build
fo test
scripts/check_text_policy.sh --self-test
scripts/check_text_policy.sh
```

The CI equivalent uses `fpm build` and `fpm test`, because `fo` is a local
workflow command and is not assumed to exist on the runner.

Every behavioral test must have an oracle independent of the implementation
under test. A test that only checks a value against another call to the same
implementation is not evidence. When practical, deliberately break the
implementation and verify that the test fails before restoring it.

## Fortran conventions

Use standard Fortran and `iso_fortran_env` kinds. Keep modules under 500 lines
and procedures under 50 lines. Return errors through values or status objects;
library procedures do not print diagnostics. Derived types end in `_t`.

Text is immutable bytes plus spans or interned IDs when it is unbounded data.
Do not use repeated allocatable-character concatenation as an accumulator.
Boundary uses of allocatable character must carry an explicit
`! text-policy: <reason>` comment and pass the text-policy gate.

New work is released under the MIT License in `LICENSE.md`.

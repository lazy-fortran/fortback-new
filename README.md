# fortback-new

The target-specific backend for the Lazy Fortran compiler-generation program.

`fortback-new` will derive target descriptions and backend machinery from
machine-readable ISA, ABI, relocation, and object-format specifications. The
repository is intended to generate encoders, decoders, register and feature
metadata, object writers, and instruction-selection support. Correctness comes
from the target specifications and independent behavioral or formal oracles.

The backend is separate from `ffc-new`, whose role is the target-independent
middle end and MIR. The eventual boundary is:

```text
ffc-new MIR → fortback-new TargetIR → native object code
```

TargetIR is a specification representation for what a machine and platform
mean. It is distinct from MIR, which represents the program being compiled.
LLVM IR and WebAssembly may serve as import, export, and differential formats,
but neither defines the production MIR.

## Initial targets

Work begins with RISC-V and AArch64 because both have strong machine-readable
architecture sources and executable or formal semantic models. x86-64 follows
with explicitly weaker provenance because its encoding and semantic sources are
more fragmented.

The principal source families are:

- RISC-V opcodes and Sail, with Spike and QEMU as behavioral oracles.
- Arm's machine-readable A-profile architecture data and ASL or Sail models,
  with QEMU and hardware as behavioral oracles.
- Intel XED and Zydis for x86-64 encoding comparison, with LLVM and hardware as
  differential or behavioral oracles.

External sources are pinned and referenced by provenance. They are not vendored
into this repository.

## Development status

This repository is being scaffolded in parallel with `lazy-fortran-new`, the
research laboratory that records the architecture, source pins, decisions,
experiments, and cross-repository wiring. The implementation roadmap is being
developed separately from this initial public scaffold.

## License

New work in this repository is released under the MIT License. See
[`LICENSE.md`](LICENSE.md). External specifications and tools retain their own
licenses.

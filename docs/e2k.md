# E2K Instruction Set Architecture (ISA) Documentation

The E2K ISA is a VLIW (Very Long Instruction Word) instruction set designed for explicit parallelism, inspired by the Elbrus 2000 architecture. This documentation provides an overview of a simplified version of the E2K ISA for educational purposes within the Wrench simulator. It focuses on core concepts like instruction bundling, predicate-based control, and static scheduling.

## Architecture Overview

The E2K architecture is a 32-bit VLIW architecture emphasizing compiler-driven parallelism. Key features include:

- Wide instructions (bundles) that group multiple independent operations for parallel execution.
- 32 general-purpose registers (%r0 to %r31), with %r0 often used as zero.
- 8 predicate registers (%pred0 to %pred7) for conditional execution without branches.
- Special registers: %ctpr1-%ctpr3 (control transfer preparation registers for calls/jumps), %wd (window descriptor for register windowing), %usd (user stack descriptor).
- Memory-mapped I/O.
- Explicit parallelism: The compiler (or programmer) schedules operations into bundles; hardware executes them in parallel without dynamic reordering.
- Security features: Basic tagged memory support (simplified in this educational version).

This architecture is suitable for learning about VLIW concepts, such as bundling operations to exploit instruction-level parallelism (ILP).

Comments in E2K assembly code are denoted by the `;` character.

## ISA Specific State Views

- `%rN:dec`, `%rN:hex` -- General-purpose registers (N=0-31).
- `%predN` -- Predicate registers (N=0-7, 1-bit values).
- `%ctprN` -- Control transfer preparation registers (N=1-3).
- `%wd` -- Window descriptor.
- `%usd` -- User stack descriptor.

## Instructions

Instruction size: Variable-length bundles, 8-byte aligned, up to 64 bytes (16 32-bit syllables). Bundles start with a header syllable specifying the types and counts of subsequent syllables (e.g., ALS for arithmetic, CS for control, LTS for literals). Operations within a bundle are executed in parallel.

Bundles are denoted in assembly by `{ ... }`, with operations separated by semicolons.

### Data Movement Instructions

- **Move (via addd)**
    - **Syntax:** `addd, <channel> <src1>, <src2>, <dst>`
    - **Description:** Move value (or add 0) using an ALU channel.
    - **Operation:** `dst <- src1 + src2` (use 0 for move).

- **Load (ld)**
    - **Syntax:** `ld, <channel> [<base> + <offset>], <dst>`
    - **Description:** Load from memory into a register.
    - **Operation:** `dst <- mem[base + offset]`.

- **Store (st)**
    - **Syntax:** `st, <channel> <src>, [<base> + <offset>]`
    - **Description:** Store register value to memory.
    - **Operation:** `mem[base + offset] <- src`.

- **Load Literal (using LTS syllable)**
    - **Syntax:** `addd, <channel> 0x0, [_ltsN <value>], <dst>` (integrated in bundle).
    - **Description:** Load a literal value via a literal syllable.
    - **Operation:** `dst <- value`.

### Arithmetic Instructions

- **Add (addd)**
    - **Syntax:** `addd, <channel> <src1>, <src2>, <dst>`
    - **Description:** 32-bit addition on specified ALU channel (0-5).
    - **Operation:** `dst <- src1 + src2`; may set overflow if extended.

- **Subtract (subd)**
    - **Syntax:** `subd, <channel> <src1>, <src2>, <dst>`
    - **Description:** 32-bit subtraction.
    - **Operation:** `dst <- src1 - src2`.

- **Multiply (muld)**
    - **Syntax:** `muld, <channel> <src1>, <src2>, <dst>`
    - **Description:** 32-bit multiplication.
    - **Operation:** `dst <- src1 * src2`.

- **Divide (divd)**
    - **Syntax:** `divd, <channel> <src1>, <src2>, <dst>`
    - **Description:** 32-bit division.
    - **Operation:** `dst <- src1 / src2`.

- **Set Predicate (setei)**
    - **Syntax:** `setei, <channel> <value>, %predN`
    - **Description:** Set a predicate register to a value (0 or 1).
    - **Operation:** `%predN <- value`.

### Bitwise Instructions

- **Shift Left (shld)**
    - **Syntax:** `shld, <channel> <src>, <shift>, <dst>`
    - **Description:** Logical shift left.
    - **Operation:** `dst <- src << shift`.

- **Shift Right (shrd)**
    - **Syntax:** `shrd, <channel> <src>, <shift>, <dst>`
    - **Description:** Arithmetic shift right (sign-extending).
    - **Operation:** `dst <- src >> shift`.

- **Bitwise AND (andd)**
    - **Syntax:** `andd, <channel> <src1>, <src2>, <dst>`
    - **Description:** Bitwise AND.
    - **Operation:** `dst <- src1 & src2`.

- **Bitwise OR (ord)**
    - **Syntax:** `ord, <channel> <src1>, <src2>, <dst>`
    - **Description:** Bitwise OR.
    - **Operation:** `dst <- src1 | src2`.

- **Bitwise XOR (xord)**
    - **Syntax:** `xord, <channel> <src1>, <src2>, <dst>`
    - **Description:** Bitwise XOR.
    - **Operation:** `dst <- src1 ^ src2`.

- **Bitwise NOT (notd)**
    - **Syntax:** `notd, <channel> <src>, <dst>`
    - **Description:** Bitwise NOT.
    - **Operation:** `dst <- ~src`.

### Control Flow Instructions

- **Prepare Call/Jump (sdisp)**
    - **Syntax:** `sdisp %ctprN, <disp>`
    - **Description:** Set displacement for control transfer in %ctprN.
    - **Operation:** `%ctprN <- pc + disp` (or absolute).

- **Call**
    - **Syntax:** `call %ctprN, wbs = <shift>`
    - **Description:** Call subroutine, shifting register window.
    - **Operation:** Jump to %ctprN, adjust %wd by shift.

- **Return (iret)**
    - **Syntax:** `iret`
    - **Description:** Return from subroutine.
    - **Operation:** Restore PC and window.

- **Branch if Predicate True (ct)**
    - **Syntax:** `ct %ctprN if %predM`
    - **Description:** Conditional transfer based on predicate.
    - **Operation:** If %predM == 1, pc <- %ctprN.

- **Wait**
    - **Syntax:** `wait`
    - **Description:** Synchronize operations (e.g., after loads).
    - **Operation:** Stall until pending ops complete.

- **Halt**
    - **Syntax:** `halt`
    - **Description:** Halt the machine.
    - **Operation:** Stop execution.

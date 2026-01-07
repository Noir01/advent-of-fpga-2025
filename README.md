# Advent of FPGA 2025

My solutions for Advent of FPGA 2025, written in OCaml using Hardcaml.

## Building

This project uses the oxcaml compiler and Jane Street's opam repository.

### Prerequisites

- opam 2.x
- System dependencies: `libgmp-dev`, `libffi-dev`, `pkg-config`, `zlib1g-dev`

On Ubuntu/Debian:
```bash
sudo apt-get install libgmp-dev libffi-dev pkg-config zlib1g-dev
```

### Setup

1. Add the oxcaml opam repository:
```bash
opam repo add ox https://github.com/oxcaml/opam-repository.git
```

2. Create a switch with the oxcaml compiler:
```bash
opam switch create . ocaml-variants.5.2.0+ox
```

3. Install dependencies:
```bash
opam install . --deps-only
```

### Build

```bash
dune build
```

### Run tests

```bash
dune runtest
```

### Run on manual inputs

First, populate inputs/. Make sure the file names are in the format `day{DD}.txt`. Then run `dune exec bin/run_day{DD}.exe`. Replace `DD` with your day.
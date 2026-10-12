# shreq

Declare a shell script's environment requirements in its own comments and check
whether the current environment satisfies them.

```bash
#!/usr/bin/env bash
# Requires: Bash >= 5.2
# Requires: GNU grep >= 3.11
# Requires: jq

set -euo pipefail
```

## Build and install

Requires OCaml 4.13 or later, Dune 3.21 or later, and a Unix-like system.
The only runtime library dependency is OCaml's Unix library.

```sh
opam install . --deps-only
dune build
dune runtest
opam install .
```

## Usage

Save the example above as `script.sh`, then run:

```console
$ shreq script.sh
✓ Bash 5.2.37 (requires >= 5.2)
✓ GNU grep 3.11 (requires >= 3.11)
✗ jq: command not found
```

Results depend on your environment: `✓` satisfied, `✗` unsatisfied, `?` unknown.

## Limitations

Checks use the environment at CLI startup and the first executable regular
file resolved through its `PATH` (including symlinks to such files). GNU grep
installed as `ggrep` does not satisfy `GNU grep` when `grep` is another
implementation. An unset `PATH` resolves no commands; empty entries mean the
current directory.

Shell functions, aliases, script-local `PATH` changes, and shell syntax are not
analyzed. Version checks execute the resolved command with `--version`, without
a shell; commands on `PATH` must be trusted. There is no probe timeout.
The MVP checks one file at a time, with no installation, configuration files,
network access, or script rewriting.

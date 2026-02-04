# rocq-git

This repository contains an implementation of [git](https://git-scm.com/) in [Rocq](https://rocq-prover.org/).
It is extracted to OCaml.

## Layout

- [theories](./theories) contains the Rocq implementation of git. [extract.v](theories/extract.v) performs extraction of the git subcommands defined in [endpoints.v](./theories/endpoints.v).
- [rocqgit](./rocqgit) contains the [extracted OCaml code](./rocqgit/lib/init.ml) as well as [the rgit executable](./rocqgit/bin/).

## Building

```bash
# Install Dependencies
opam switch create rocq 4.14.1
opam pin add rocq-runtime 9.1.0
opam install rocq-prover dune

# Clone and build
git clone https://github.com/CharlesAverill/rocq-git && cd rocq-git
dune build
```

# The Flambda backend project for OCaml

This repository is for more experimental work, of production quality, on the middle end
and backend of the OCaml compiler.
This is also the home of the Flambda 2 optimiser and the Cfg backend.

The Flambda backend is currently based on OCaml 5.2.0 (plus some patches from later
upstream revisions, mainly in the runtime).  It supports both the OCaml 4 and OCaml 5
runtime systems, although support for the OCaml 4 runtime is expected to be removed
in late 2024.

The following gives basic instructions for getting set up.  Please see
[`HACKING.md`](HACKING.md) for more detailed instructions if you want to develop in this repo.
That file also contains instructions for installing the Flambda backend compiler in a way
that it can be used to build OPAM packages.

## One-time setup for dev work or installation

Only currently tested on Linux/x86-64 and macOS/x86-64.

One-time setup (you can also use other 4.14.x releases):
```
$ opam switch 4.14.1  # or "opam switch create 4.14.1" if you haven't got that switch already
$ eval $(opam env)
$ opam install dune.3.15.2 menhir.20231231
```

You probably then want to fork the `ocaml-flambda/flambda-backend` repo to your own Github org.

## Branching and configuring

Use normal commands to make a branch from the desired upstream branch (typically `main`), e.g.:
```
$ git clone https://github.com/ocaml-flambda/flambda-backend
$ cd flambda-backend
$ git checkout -b myfeature origin/main
```

The Flambda backend tree has to be configured before building.  The configure script is not checked
in; you have to run `autoconf`.  For example:
```
$ autoconf
$ ./configure --prefix=/path/to/install/dir
```

## Building and installing

To build and install the Flambda backend, which produces a compiler installation directory whose
layout is compatible with upstream, run:
```
$ make install
```
test change

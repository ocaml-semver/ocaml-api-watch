# CONTRIBUTING

## Setting up your dev environment

If you would like to contribute to `ocaml-api-watch` you will need to setup
your dev environment first. You can follow the steps described below.

First you need to clone the project locally on your machine:
```
git clone git@github.com:NathanReb/ocaml-api-watch.git
```

Now go into your local copy of the project:
```
cd ocaml-api-watch
```

And setup a local opam switch to install the right ocaml version along with the
set of base dependencies:
```
opam switch create ./ 4.12.0 --deps-only -t
```

You should also install `ocamlformat` and `merlin` for a better dev experience
```
opam install ocamlformat.0.18.0 merlin
```

From there you should be all set. You can run the following commands to build
the project:
```
dune build
```

To run the test suite:
```
dune runtest
```

To format the source and dune files:
```
dune build @fmt --auto-promote
```

## Submitting your first contribution

Before submitting a contribution, you will need to have your own fork of the
project. You can create it by following
[this link](https://github.com/NathanReb/ocaml-api-watch/fork).

Add your fork as a git remote to your local git repository:
```
git remote add my-fork git@github.com:<your-github-username>/ocaml-api-watch.git
```

Before working on your patch, make sure your main branch is up to date:
```
git checkout main
git pull
```

You can now create a new branch where you can work on your changes:
```
git checkout -b <branch-name>
```

As you work on your changes, try to ensure that every commit builds and is
correctly formatted. This will make it easier for maintainer to browse the
history when they are looking for the source of the bug later on for instance.
To do that, ensure that before you commit, you run the following commands:
```
dune build @fmt --auto-promote
dune build
```
and that they exit successfully.

Once your patch is complete, push your branch to your fork:
```
git push -u my-fork <branch-name>
```

Then head to the github project page, it should suggest you to open a PR
for your newly updated branch.

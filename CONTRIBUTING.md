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
opam switch create ./ --deps-only -t --with-dev-setup
```

You should also install `merlin` and `ocp-indent` for a better dev experience
```
opam install merlin ocp-indent
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

## Writing tests

This repo uses dune's cram tests feature for testing the provided tools and
expect tests for testing our library functions.

### Cram tests

We use cram tests to test our command line tools.

Cram tests can be found in the `tests/api-diff` folder and are written in `.t`
files. Those files are made out of text which is usually here to describe what
the actual tests are doing and provide a bit of context.
Among this text are also indented parts which are made of commands to execute
and their expected output.

For example:
```
In this test we will list the files available in the current directory.
We should see two files

  $ ls
  some_file some_other_file
```

The two first lines are just descriptive text for the test.

The next non empty lines are
```
  $ ls
```
which is the command to run. Command lines are indented and start with a `$`.
The line right below that is the command's expected output.

When running the project test suite with
```
dune runtest
```

dune will execute those commands in a controlled environment and compare their
output with the expected one.

If all tests pass there will be no output to `dune runtest`. If some fails
though, you will see a few diffs like this:
```
Done: 22/24 (jobs: 1)File "tests/api-watch/run.t", line 1, characters 0-0:
         git (internal) (exit 1)
(cd _build/.sandbox/c19c63dd297f4f6a5ad4536031b70330/default && /usr/bin/git --no-pager diff --no-index --color=always -u ../../../default/tests/api-watch/run.t tests/api-watch/run.t.corrected)
diff --git a/../../../default/tests/api-watch/run.t b/tests/api-watch/run.t.corrected
index 58d9cd4..520d3bf 100644
--- a/../../../default/tests/api-watch/run.t
+++ b/tests/api-watch/run.t.corrected
@@ -27,4 +27,4 @@ In this test we will list the files available in the current directory.
 We should see two files

   $ ls
-  some_file some_other_file
+  some_file some_file2
```

That means the command output was different from the expected one. You should
fix the test until you have no diffs left.

Sometimes though, it is expected that the output changes, for instance if the
tool prints things in a different format than it used to. In that case, once you
are sure the new output is correct according to your changes and what the test
should show, you can accept this new version by running:
```
dune promote
```

This will update the expected output directly in the original `.t` file.

#### Generating files on the fly

It is often the case that when you write a test you will need input files for
it. You could add those files to the repository and use them in the test but
that results in a potentially large number of test files lying around and it
also make it harder to read the test since you have to open several files to get
the whole picture.

What you can do instead is generate those files as part of the test, by using
a bash' heredoc redirection. It's usually done like this:

```
For this test we need the following input file

  $ cat > ref.mli << EOF
  > type t = int
  >
  > val f : t -> string
  > EOF

```

Here, we generate a `ref.mli` file which contains the following:
```
type t = int

val f : t -> string
```

the `<< EOF` part indicates that we use `EOF` as an end delimiter for our file.

### Expect tests

Expect tests can be considered as the OCaml counterpart to cram tests. They use
the same promotion workflow as cram tests: they contain the expected test
output, if the test runner produces a different output from the one in the test
file, it will show the diff. At this point you can either accept the change and
promote it to the test file or try to fix the code until the output matches
depending on the nature of the change.

We use `ppx_expect` as our expect test runner, you can read the documentation on
the project homepage [here](https://github.com/janestreet/ppx_expect).

Our expect tests live in the `tests/api-watch` folder. They are written in
regular `.ml` files.

Imagine we want to test a simple `add` function that returns the sum of two
integers, here's how we would go about it:
```
let%expect_test "0 is neutral" =
  let res = add 2 0 in
  Format.printf "%d" res;
  [%expect 2]
```

As you can see expected are written in a `let%expect_test "<test-name>" = ...`
let binding. You can define regular functions and values outside of those blocks
in an expect test if you need to factor out some code.

The test usually contain one or multiple calls to the function under test
followed by printing statements and `[%expect <expected output>]` statements.
The test runner will capture the output after executing the test block and
compare it with the content of the `[%expect ...]` statements.

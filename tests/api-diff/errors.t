api-diff only accepts arguments of the same nature, that is it either
diff a .cmi file with another .cmi file or a directory with a directory

  $ mkdir test
  $ touch test.cmi
  $ api-diff test test.cmi
  api-diff: Arguments must either both be directories or both single .cmi files.
  [123]

When diffing all libraries, the --main-module argument is mandatory

  $ mkdir test2
  $ api-diff test test2
  api-diff: --main-module must be provided when diffing entire libraries.
  [123]

When passing --main-module while diffing single .cmi files, the user will be warn
that it is ignored

  $ touch test2.cmi
  $ api-diff --main-module main test.cmi test2.cmi
  api-diff: --main-module ignored when diffing single .cmi files
  api-diff: Cmi_format.Error(_)
  [123]
